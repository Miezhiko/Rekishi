module Main where

import           Config     (getCfg)
import           Historical (runHistoricalClient)

main ∷ IO ()
main = getCfg >>= runHistoricalClient
