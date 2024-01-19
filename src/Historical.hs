module Historical where

import           Config                         (Config (cfgToken))

import           Data.Foldable                  (for_)
import           Data.ProtoLens.Message
import qualified Data.Text                      as T

import           Invest.Client
import           Invest.Service.Operations      (getPortfolio)
import           Invest.Service.Users           (getAccounts)

import qualified Proto.Invest.Common_Fields     as C
import           Proto.Invest.Operations
import qualified Proto.Invest.Operations_Fields as O
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
  let accId = acc ^. O.id
      pr    = build (O.accountId .~ accId)
  pf <- runGetPortfolio g pr
  let positions = pf ^. O.positions
  for_ positions $ \pos ->
    putStrLn $ "F: " ++ T.unpack( pos ^. O.figi )
          ++ "\tQ: " ++ show ( pos ^. O.quantity ^. C.units )
          ++ "\tP: " ++ show ( pos ^. O.currentPrice ^. C.units )
getAccountStuff g (x:_) = getAccountStuff g [x]

runHistoricalClient ∷ Config -> IO ()
runHistoricalClient cfg = do
  let config = ClientConfig {
    token = (cfgToken cfg),
    appName = Nothing
  }

  client    <- runClient config
  accounts  <- runGetAccounts client
  getAccountStuff client accounts
