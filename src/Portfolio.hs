module Portfolio
  ( runPortfolio
  ) where

import           Base
import           Figi

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
    let figi = pos ^. O.figi
    ticker      <- figiToTicker figi
    (lot, _)    <- tickerToLot ticker
    let currPrice = pos ^. O.currentPrice ^. C.units
        realPrice = currPrice * (fromIntegral lot)
        quantity  = pos ^. O.quantity ^. C.units
    pure $ summ + realPrice * quantity) 0 positions
  for_ positions $ \pos -> do
    let figi = pos ^. O.figi
    ticker      <- figiToTicker figi
    (lot, curr) <- tickerToLot ticker
    let currPrice = pos ^. O.currentPrice ^. C.units
        realPrice = currPrice * (fromIntegral lot)
        quantity  = pos ^. O.quantity ^. C.units
    putStrLn $ "F: " ++ T.unpack figi
          ++ "\tT: " ++ T.unpack ticker
          ++ "\tQ: " ++ show quantity
          ++ "\tP: " ++ show ( realPrice ) ++ " " ++ (T.unpack curr)
          ++ "\tA: " ++ show ( realPrice * quantity ) ++ " " ++ (T.unpack curr)
  putStrLn $ "Total Cap: " ++ show total
getAccountStuff g (x:_) = getAccountStuff g [x]

runPortfolio ∷ GrpcClient -> IO ()
runPortfolio client =
  runGetAccounts client
    >>= getAccountStuff client
