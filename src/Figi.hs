{-# LANGUAGE
    KindSignatures
  , RankNTypes
  , FlexibleContexts
  #-}

module Figi
  ( figiToTicker
  , loadBaseShares
  , tickerToFigi
  ) where

import           Base
import           State

import qualified Data.Map                        as M
import           Data.ProtoLens.Message
import qualified Data.Text                       as T
import           Data.Tuple                      (swap)

import qualified Data.ProtoLens.Field as F

import           Invest.Client
import           Invest.Service.Instruments      (bonds, currencies, etfs, futures, shares)

import           Proto.Invest.Instruments
import qualified Proto.Invest.Instruments_Fields as I

getBaseShares ∷ GrpcClient -> GrpcIO ([Share], [Currency], [Bond], [Future], [Etf])
getBaseShares gc = do
  myShares      <- shares     gc (defMessage & I.instrumentStatus .~ INSTRUMENT_STATUS_ALL)
  myCurrencies  <- currencies gc (defMessage & I.instrumentStatus .~ INSTRUMENT_STATUS_ALL)
  myBonds       <- bonds      gc (defMessage & I.instrumentStatus .~ INSTRUMENT_STATUS_BASE)
  myFutures     <- futures    gc (defMessage & I.instrumentStatus .~ INSTRUMENT_STATUS_BASE)
  myEtf         <- etfs       gc (defMessage & I.instrumentStatus .~ INSTRUMENT_STATUS_ALL)
  pure $ (myShares, myCurrencies, myBonds, myFutures, myEtf)

getMap :: ∀ r. (
  (F.HasField r "figi" T.Text),
  (F.HasField r "ticker" T.Text)
  ) => [r] -> ([(T.Text, T.Text)], [(T.Text, T.Text)])
getMap xs = let figiTicker = map (\s -> (s ^. I.figi, s ^. I.ticker)) xs
            in (figiTicker, (map swap figiTicker))

loadBaseShares ∷ GrpcClient -> IO ()
loadBaseShares client = do
  (s, c, b, f, e) <- runExceptT (getBaseShares client) >>= \case
    Left err -> error ∘ show $ err
    Right sh -> pure sh
  writeIORef stateShares s
  let (stk, str) = getMap s
      (ctk, ctr) = getMap c
      (btk, btr) = getMap b
      (ftk, ftr) = getMap f
      (etk, etr) = getMap e
  writeIORef stateTickers $ M.fromList ( stk ++ ctk ++ btk ++ ftk ++ etk )
  writeIORef stateFigis   $ M.fromList ( str ++ ctr ++ btr ++ ftr ++ etr )

figiToTicker ∷ T.Text -> IO T.Text
figiToTicker figi = do
  tickers <- readIORef stateTickers
  case M.lookup figi tickers of
    Just ti -> pure ti
    Nothing -> pure $ T.pack( "FIGI: " ++ (T.unpack figi) )

tickerToFigi ∷ T.Text -> IO T.Text
tickerToFigi figi = do
  tickers <- readIORef stateFigis
  case M.lookup figi tickers of
    Just ti -> pure ti
    Nothing -> pure $ T.pack( "Ticker: " ++ (T.unpack figi) )
