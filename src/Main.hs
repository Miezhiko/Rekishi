module Main where

import           Config       (getCfg)
-- import           MarketDataStreamClient (runMarketDataStreamClient)
-- import           SandBoxClient          (runSandBoxClient)
import           SimpleClient (runSimpleClient)
import           Historical (runHistoricalClient)

main ∷ IO ()
main = do
  cfg <- getCfg

  -- runSimpleClient cfg

  runHistoricalClient cfg

  -- runSandBoxClient cfg

  -- runMarketDataStreamClient cfg
