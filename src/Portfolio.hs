module Portfolio
  ( runPortfolio
  ) where

import           Base
import           Figi
import           Types

import           Text.Printf

import           Data.Foldable                  (for_)
import           Data.Maybe                     (catMaybes)
import           Data.ProtoLens.Message
import qualified Data.Text                      as T

import           Invest.Client
import           Invest.Service.Operations      (getPortfolio)
import           Invest.Service.Users           (getAccounts)

import qualified Proto.Invest.Common_Fields     as C
import           Proto.Invest.Operations
import qualified Proto.Invest.Operations_Fields as O
import           Proto.Invest.Users

runGetAccounts ∷ GrpcClient -> IO [Account]
runGetAccounts client = runExceptT (getAccounts client) >>= \case
  Left err -> error ∘ show $ err
  Right ac -> pure ac

runGetPortfolio ∷ GrpcClient -> PortfolioRequest -> IO PortfolioResponse
runGetPortfolio client pr =
  runExceptT (getPortfolio client pr) >>= \case
    Left err -> error ∘ show $ err
    Right pf -> pure pf

getAccountStuff ∷ GrpcClient -> [Account] -> IO ()
getAccountStuff _ []    = putStrLn "no accounts found for config"
getAccountStuff g [acc] = do
  let accId = acc ^. O.id
      pr    = build $ O.accountId .~ accId
  pf <- runGetPortfolio g pr
  let positions = pf ^. O.positions
  reSharesMb <- traverse (\pos -> do
    let myFigi = pos ^. O.figi
    reShare <- figiToReShare myFigi
    lsPrice <- figiToLastPrice myFigi
    case reShare of
      Just re -> do
        let realPrice =
              case lsPrice of
                Just (сUn, cNan) ->(fromIntegral сUn) + cNan
                Nothing ->
                  let myUnits = fromIntegral $ pos ^. O.currentPrice ^. C.units
                      myNanos = fromIntegral $ pos ^. O.currentPrice ^. C.nano
                      (cUn, cNan) = (myUnits, ( myNanos / 1000000000 :: Float ))
                  in cUn * (fromIntegral (lot re)) + cNan
            quantity  = fromIntegral $ pos ^. O.quantity ^. C.units :: Int
        pure $ Just (re, realPrice, quantity)
      Nothing -> pure Nothing) positions
  dollarPriceMb <- figiToLastPrice $ T.pack dollarFigi
  let dollarPrice = case dollarPriceMb of
                      Just (u, n) -> (fromIntegral u) + n
                      Nothing     -> 90.0 :: Float -- fair?
      reShares = catMaybes reSharesMb
      total = foldl (\summ (re, realPrice, quantity) ->
                  let sCurrency = T.unpack $ currency re
                      rubPrice  = if sCurrency == "usd"
                                   then realPrice * dollarPrice
                                   else realPrice
                      newToSumm = rubPrice * (fromIntegral quantity) :: Float
                  in summ + newToSumm) 0.0 reShares
      maxQl = maximum $ map (\(_, _, q) -> length (show q)) reShares
  for_ reShares $ \(re, realPrice, quantity) -> do
    let sCurrency = T.unpack $ currency re
        quantityS = show quantity
        quantityP = maxQl - length quantityS
        quantityA = concat $ replicate quantityP " "
        rubPrice  = if sCurrency == "usd"
            then realPrice * dollarPrice
            else realPrice
        totalPrice = rubPrice * (fromIntegral quantity)
    putStrLn $ "T: " ++ ( T.unpack ( figi re ) ) --take 4 ( T.unpack ( ticker re ) )
          ++ "\tQ: " ++ quantityS ++ quantityA
          ++ "\tP: " ++ printf "%.2f" ( realPrice ) ++ " " ++ sCurrency
          ++ "\tA: " ++ show ( round totalPrice :: Int ) ++ " rub"
          ++ "\tN: " ++ T.unpack ( name re )
  putStrLn $ "Total Cap: " ++ show total
getAccountStuff g (x:_) = getAccountStuff g [x]

runPortfolio ∷ GrpcClient -> IO ()
runPortfolio client =
  runGetAccounts client
    >>= getAccountStuff client
