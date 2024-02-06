module Portfolio
  ( runPortfolio
  ) where

import           Base
import           Figi
import           Types

import           Data.Foldable                  (for_)
import           Data.ProtoLens.Message
import qualified Data.Text                      as T

import           Control.Monad                  (foldM)

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
  total <- foldM (\summ pos -> do
    let myFigi = pos ^. O.figi
    reShare <- figiToReShare myFigi
    case reShare of
      Just re -> let currPrice = pos ^. O.currentPrice ^. C.units
                     realPrice = (fromIntegral currPrice) * (lot re)
                     quantity  = fromIntegral $ pos ^. O.quantity ^. C.units
                 in pure $ summ + realPrice * quantity
      Nothing -> pure summ) 0 positions
  for_ positions $ \pos -> do
    let myFigi = pos ^. O.figi
    reShare <- figiToReShare myFigi
    case reShare of
      Just re -> do
        let currPrice = pos ^. O.currentPrice ^. C.units
            realPrice = (fromIntegral currPrice) * (lot re)
            quantity  = fromIntegral $ pos ^. O.quantity ^. C.units
            sCurrency = T.unpack $ currency re
        putStrLn $ "T: " ++ take 4 ( T.unpack ( ticker re ) )
              ++ "\tQ: " ++ show quantity
              ++ "\tP: " ++ show ( realPrice ) ++ " " ++ sCurrency
              ++ "\tA: " ++ show ( realPrice * quantity ) ++ " " ++ sCurrency
              ++ "\tN: " ++ T.unpack ( name re )
      Nothing -> putStrLn $ "Error on " ++ ( T.unpack myFigi )
  putStrLn $ "Total Cap: " ++ show total
getAccountStuff g (x:_) = getAccountStuff g [x]

runPortfolio ∷ GrpcClient -> IO ()
runPortfolio client =
  runGetAccounts client
    >>= getAccountStuff client
