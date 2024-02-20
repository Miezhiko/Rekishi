{-# LANGUAGE
    KindSignatures
  , RankNTypes
  #-}

module Main where

import           Base
import           Config                (Config (cfgToken), getCfg)
import           Figi                  (loadBaseShares, restoreCache, storeCache)
import           Historical            (runHistorical)
import           Portfolio             (runPortfolio)
import           Ticker                (runTicker)
import           Version

import           Data.Kind

import           System.Console.GetOpt
import           System.Directory
import           System.Environment    (getArgs)
import           System.Exit
import           System.FilePath       ((</>))
import           System.IO

import           Client

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
  Option "h" ["historical"] (NoArg getH)                "Display Historical Data",
  Option "t" ["ticker"]     (ReqArg gett "String")      "Display Some Ticker History"
  ]

withConfig ∷ IO GrpcClient
withConfig = do
  myCfg <- getCfg
  let config = ClientConfig {
    token = (cfgToken myCfg),
    appName = Nothing
  }
  runClient config

getRCachePath ∷ IO FilePath
getRCachePath = (</> "rekishi.cache") <$> getHomeDirectory

runPortfolioExec ∷ IO ()
runPortfolioExec =
  getRCachePath >>= \rCachePath ->
    withBinaryFile rCachePath ReadWriteMode $ \h -> do
      client <- withConfig
      mySharesState <- loadBaseShares client
      runPortfolio client
      storeCache h mySharesState

runHistoricalExec ∷ IO ()
runHistoricalExec = withConfig >>= runHistorical

runTickerExec ∷ String -> IO ()
runTickerExec ticker =
  getRCachePath >>= \rCachePath ->
    withBinaryFile rCachePath ReadWriteMode $ \h -> do
      client <- withConfig
      restoreCache client h
      runTicker client ticker

gett ∷ ∀ (μ :: Type -> Type). Monad μ => String -> Options -> μ Options
gett arg ο = pure ο { optRekishi = runTickerExec arg }

getP ∷ ∀ τ β. τ -> IO β
getP _ = runPortfolioExec >> exitSuccess

getH ∷ ∀ τ β. τ -> IO β
getH _ = runHistoricalExec >> exitSuccess
