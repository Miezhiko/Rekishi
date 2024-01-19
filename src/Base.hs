module Base
  ( module Export
  , runClient
  ) where

import           Prelude.Unicode as Export

import           Invest.Client

runClient ∷ ClientConfig -> IO GrpcClient
runClient cnfg = runExceptT (initGrpcClient cnfg) >>= \case
  Left err -> error ∘ show $ err
  Right gc -> pure gc
