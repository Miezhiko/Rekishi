module SandBoxClient where

import           Config                    (Config (cfgToken))

import           Control.Monad             (void)

import           Data.Text                 (Text)

import           Client
import           Service.Sandbox

import           Proto.Users_Fields as U

-- Creates new sandbox account if there is no one
getSandBoxAccountId ∷ GrpcClient -> GrpcIO Text
getSandBoxAccountId gc = getSandboxAccounts gc >>= \case
  []  -> openSandboxAccount gc
  arr -> pure $ head arr ^. U.id

runSandBoxClient ∷ Config -> IO ()
runSandBoxClient cfg = void . runExceptT $ do
  client <- initClient
  myAccountId <- getSandBoxAccountId client
  orders <- getSandboxOrders client myAccountId
  liftIO . print $ orders
  where initClient = initGrpcClient $ ClientConfig {
    token = (cfgToken cfg),
    appName = Nothing
  }
