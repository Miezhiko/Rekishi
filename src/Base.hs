module Base
  ( module Export
  , dollarFigi
  , runClient
  ) where

import           Prelude.Unicode as Export

import           Invest.Client

dollarFigi ∷ String
dollarFigi = "BBG0013HGFT4"

runClient ∷ ClientConfig -> IO GrpcClient
runClient cnfg = runExceptT (initGrpcClient cnfg) >>= \case
  Left err -> error ∘ show $ err
  Right gc -> pure gc
