module Base
  ( module Export
  , defaultCurrency
  , dollarFigi
  , runClient
  ) where

import           Prelude.Unicode as Export

import           Invest.Client

dollarFigi ∷ String
dollarFigi = "BBG0013HGFT4"

defaultCurrency ∷ String
defaultCurrency = "rub"

runClient ∷ ClientConfig -> IO GrpcClient
runClient cnfg = runExceptT (initGrpcClient cnfg) >>= \case
  Left err -> error ∘ show $ err
  Right gc -> pure gc
