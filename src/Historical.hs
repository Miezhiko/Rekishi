module Historical where

import           Config                          (Config (cfgToken))

import           Control.Monad                   (void)

import qualified Data.Text                       as T

import           Invest.Client
import           Invest.Service.Instruments      (shares)
import           Invest.Service.MarketData
import           Invest.Service.Operations       (getPortfolio)
import           Invest.Service.Users            (getAccounts)

import           Proto.Invest.Instruments
import qualified Proto.Invest.Instruments_Fields as I
import           Proto.Invest.Marketdata
import qualified Proto.Invest.Marketdata_Fields  as MD
import           Proto.Invest.Operations
import           Proto.Invest.Users

runClient ∷ ClientConfig -> IO GrpcClient
runClient cnfg = runExceptT (initGrpcClient cnfg) >>= \case
  Left err -> error . show $ err
  Right gc -> pure gc

runGetAccounts ∷ GrpcClient -> IO [Account]
runGetAccounts client = runExceptT (getAccounts client) >>= \case
  Left err -> error . show $ err
  Right ac -> pure ac

runGetPortfolio ∷ GrpcClient -> PortfolioRequest -> IO PortfolioResponse
runGetPortfolio client pr =
  runExceptT (getPortfolio client pr) >>= \case
    Left err -> error . show $ err
    Right pf -> pure pf

getAccountStuff ∷ GrpcClient -> [Account] -> IO ()
getAccountStuff _ []    = putStrLn "no accounts found for config"
getAccountStuff g [acc] = do
  --let accId = acc ^. id
  -- let pr = PortfolioRequest "0"
  -- pf <- runGetPortfolio g pr
  print acc
getAccountStuff g (x:_) = getAccountStuff g [x]

-- Gets base shares prices and prints the first one
runHistoricalClient ∷ Config -> IO ()
runHistoricalClient cfg = do
  let config = ClientConfig {
    token = (cfgToken cfg),
    appName = Nothing
  }

  client    <- runClient config
  accounts  <- runGetAccounts client
  getAccountStuff client accounts
