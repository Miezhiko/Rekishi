{-# LANGUAGE
    KindSignatures
  , RankNTypes
  #-}

module Main where

import           Base
import           Config                (Config (cfgToken), getCfg)
import           Figi                  (loadBaseShares)
import           Portfolio             (runPortfolio)
import           Historical            (runHistorical)
import           Version

import           System.Console.GetOpt
import           System.Environment    (getArgs)
import           System.Exit

import           Invest.Client

main ∷ IO ()
main = do (actions, _, _) <- getOpt RequireOrder options <$> getArgs
          Options { optRekishi = run
                  } <- foldl (>>=) (pure defaultOptions) actions
          run

newtype Options
  = Options { optRekishi :: IO () }

defaultOptions ∷ Options
defaultOptions = Options {
    optRekishi = runPortfolioExec
  }

options ∷ [OptDescr (Options -> IO Options)]
options = [
  Option "v" ["version"]    (NoArg showV)               "Display Version",
  Option []  ["help"]       (NoArg (showHelp options))  "Display Help",
  Option "p" ["portfolio"]  (NoArg getP)                "Display Portfolio",
  Option "h" ["historical"] (NoArg getH)                "Display Historical Data"
  ]

runPortfolioExec ∷ IO ()
runPortfolioExec = do
  myCfg <- getCfg
  let config = ClientConfig {
    token = (cfgToken myCfg),
    appName = Nothing
  }

  client <- runClient config

  loadBaseShares client
  runPortfolio client

runHistoricalExec ∷ IO ()
runHistoricalExec = do
  myCfg <- getCfg
  let config = ClientConfig {
    token = (cfgToken myCfg),
    appName = Nothing
  }

  client <- runClient config

  runHistorical client

getP ∷ ∀ τ β. τ -> IO β
getP _ = runPortfolioExec >> exitSuccess

getH ∷ ∀ τ β. τ -> IO β
getH _ = runHistoricalExec >> exitSuccess
