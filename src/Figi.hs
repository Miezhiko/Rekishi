{-# LANGUAGE
    FlexibleContexts
  , KindSignatures
  , RankNTypes
  #-}

module Figi
  ( figiToTicker
  , loadBaseShares
  , tickerToFigi
  , tickerToLot
  ) where

import           Base
import           State

import qualified Data.Map                        as M
import           Data.ProtoLens.Message
import qualified Data.Text                       as T

import qualified Data.ProtoLens.Field            as F

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

getFigiTickerMap ∷ ∀ r. (
  (F.HasField r "figi" T.Text),
  (F.HasField r "ticker" T.Text),
  (F.HasField r "name" T.Text)
  ) => [r] -> ([(T.Text, (T.Text, T.Text))], [(T.Text, T.Text)])
getFigiTickerMap xs =
  let figiTicker = map (\s -> (s ^. I.figi, ( s ^. I.ticker
                                            , s ^. I.name ))) xs
      tickerFigi = map (\s -> (s ^. I.ticker, s ^. I.figi)) xs
  in (figiTicker, tickerFigi)

getTickerLotMap ∷ ∀ r. (
  (F.HasField r "ticker" T.Text),
  (F.HasField r "lot" Int32),
  (F.HasField r "currency" T.Text)
  ) => [r] -> [(T.Text, (Int32, T.Text))]
getTickerLotMap = map (\s -> ( s ^. I.ticker
                           , ( s ^. I.lot, s ^. I.currency )
                           )
                      )

loadBaseShares ∷ GrpcClient -> IO ()
loadBaseShares client = do
  (s, c, b, f, e) <- runExceptT (getBaseShares client) >>= \case
    Left err -> error ∘ show $ err
    Right sh -> pure sh
  writeIORef stateShares s
  let (stk, str) = getFigiTickerMap s
      (ctk, ctr) = getFigiTickerMap c
      (btk, btr) = getFigiTickerMap b
      (ftk, ftr) = getFigiTickerMap f
      (etk, etr) = getFigiTickerMap e
  writeIORef stateTickers $ M.fromList ( stk ++ ctk ++ btk ++ ftk ++ etk )
  writeIORef stateFigis   $ M.fromList ( str ++ ctr ++ btr ++ ftr ++ etr )
  writeIORef stateLots    $ M.fromList ( getTickerLotMap s ++ getTickerLotMap c
                                      ++ getTickerLotMap b ++ getTickerLotMap f
                                      ++ getTickerLotMap e )

figiToTicker ∷ T.Text -> IO (T.Text, T.Text)
figiToTicker figi = do
  tickers <- readIORef stateTickers
  case M.lookup figi tickers of
    Just ti -> pure ti
    Nothing -> pure ( T.pack( "FIGI: " ++ (T.unpack figi) ), T.pack [] )

tickerToFigi ∷ T.Text -> IO T.Text
tickerToFigi ticker = do
  figis <- readIORef stateFigis
  case M.lookup ticker figis of
    Just ti -> pure ti
    Nothing -> pure $ T.pack( "Ticker: " ++ (T.unpack ticker) )

tickerToLot ∷ T.Text -> IO (Int32, T.Text)
tickerToLot ticker = do
  lots <- readIORef stateLots
  case M.lookup ticker lots of
    Just lt -> pure lt
    Nothing -> pure (0, T.pack "rub")
