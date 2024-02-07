{-# LANGUAGE
    FlexibleContexts
  , KindSignatures
  , RankNTypes
  #-}

module Figi
  ( figiToLastPrice
  , figiToReShare
  , loadBaseShares
  , tickerToFigi
  ) where

import           Base
import           State
import           Types

import qualified Data.Map                        as M
import           Data.ProtoLens.Message
import qualified Data.Text                       as T

import qualified Data.ProtoLens.Field            as F

import           Invest.Client
import           Invest.Service.Instruments      (bonds, currencies, shares)
import           Invest.Service.MarketData       (getLastPrices)

import           Proto.Invest.Common
import qualified Proto.Invest.Common_Fields      as C
import           Proto.Invest.Instruments
import qualified Proto.Invest.Instruments_Fields as I
import           Proto.Invest.Marketdata
import qualified Proto.Invest.Marketdata_Fields  as MD

getBaseShares ∷ GrpcClient -> GrpcIO ([Share], [Currency], [Bond]) --, [Future], [Etf]
getBaseShares gc = do
  myShares      <- shares     gc (defMessage & I.instrumentStatus .~ INSTRUMENT_STATUS_BASE)
  myCurrencies  <- currencies gc (defMessage & I.instrumentStatus .~ INSTRUMENT_STATUS_BASE)
  myBonds       <- bonds      gc (defMessage & I.instrumentStatus .~ INSTRUMENT_STATUS_BASE)
  -- myFutures     <- futures    gc (defMessage & I.instrumentStatus .~ INSTRUMENT_STATUS_BASE)
  -- myEtf         <- etfs       gc (defMessage & I.instrumentStatus .~ INSTRUMENT_STATUS_ALL)
  pure $ (myShares, myCurrencies, myBonds) --, myFutures, myEtf

getReshare ∷ ∀ r. (
  (F.HasField r "figi" T.Text),
  (F.HasField r "ticker" T.Text),
  (F.HasField r "name" T.Text),
  (F.HasField r "lot" Int32),
  (F.HasField r "currency" T.Text),
  (F.HasField r "dlong" Quotation),
  (F.HasField r "dshort" Quotation),
  (F.HasField r "nominal" MoneyValue),
  (F.HasField r "apiTradeAvailableFlag" Bool)
  ) => r -> ReShare
getReshare s =
  let sLong     = s ^. I.dlong
      sShort    = s ^. I.dshort
      sNominal  = s ^. I.nominal
  in ReShare  ( s ^. I.figi                   )
              ( s ^. I.ticker                 )
              ( fromIntegral ( s ^. I.lot )   )
              ( s ^. I.currency               )
              ( ReQuotation ( fromIntegral ( sLong ^. C.units )     )
                            ( fromIntegral ( sLong ^. C.nano )      ) )
              ( ReQuotation ( fromIntegral ( sShort ^. C.units )    )
                            ( fromIntegral ( sShort ^. C.nano )     ) )
              ( ReMoneyValue ( sNominal ^. C.currency               )
                             ( fromIntegral ( sNominal ^. C.units ) )
                             ( fromIntegral ( sNominal ^. C.nano )  ) )
              ( s ^. I.name                   )
              ( s ^. I.apiTradeAvailableFlag  )

getReshares ∷ ∀ r. (
  (F.HasField r "figi" T.Text),
  (F.HasField r "ticker" T.Text),
  (F.HasField r "name" T.Text),
  (F.HasField r "lot" Int32),
  (F.HasField r "currency" T.Text),
  (F.HasField r "dlong" Quotation),
  (F.HasField r "dshort" Quotation),
  (F.HasField r "nominal" MoneyValue),
  (F.HasField r "apiTradeAvailableFlag" Bool)
  ) => [r] -> ([ReShare], [(T.Text, ReShare)], [(T.Text, T.Text)])
getReshares xs =
  let reShares    = map getReshare xs
      reSharesMap = map (\r -> (figi r, r)) reShares
      tickerFigi  = map (\r -> (ticker r, figi r)) reShares
  in (reShares, reSharesMap, tickerFigi)

getSharesLastPrices ∷ GrpcClient -> [ReShare] -> GrpcIO [LastPrice]
getSharesLastPrices gc myShares = toLastPrices (map figi myShares)
  where toLastPrices figis = getLastPrices gc (defMessage & MD.figi .~ figis)

loadBaseShares ∷ GrpcClient -> IO ()
loadBaseShares client = do
  (s, c, b) <- runExceptT (getBaseShares client) >>= \case
    Left err -> error ∘ show $ err
    Right sh -> pure sh
  let (ss, stk, str) = getReshares s
      (cc, ctk, ctr) = getReshares c
      (bb, btk, btr) = getReshares b
      -- (ff, ftk, ftr) = getReshares f
      -- (ee, etk, etr) = getReshares e
      reShares       = ss ++ cc ++ bb -- ++ ff ++ ee 
  writeIORef stateShares reShares
  -- this method has limit for 3000 elements
  -- we use INSTRUMENT_STATUS_BASE for now
  -- also we run it separately for currencies and shares
  lastPricesS <- runExceptT (getSharesLastPrices client ss) >>= \case
                  Left err -> error ∘ show $ err
                  Right pr -> pure pr
  lastPricesC <- runExceptT (getSharesLastPrices client cc) >>= \case
                  Left err -> error ∘ show $ err
                  Right pr -> pure pr
  let priceMap = map (\p -> ( p ^. MD.figi
                            , fromIntegral (p ^. MD.price ^. C.units) )
                     ) $ lastPricesS ++ lastPricesC
  writeIORef statePrices  $ M.fromList priceMap
  writeIORef stateTickers $ M.fromList ( stk ++ ctk ++ btk ) -- ++ ftk ++ etk
  writeIORef stateFigis   $ M.fromList ( str ++ ctr ++ btr ) -- ++ ftr ++ etr

figiToReShare ∷ T.Text -> IO (Maybe ReShare)
figiToReShare myFigi = do
  tickers <- readIORef stateTickers
  case M.lookup myFigi tickers of
    Just ti -> pure $ Just ti
    Nothing -> pure Nothing

tickerToFigi ∷ T.Text -> IO T.Text
tickerToFigi myTicker = do
  figis <- readIORef stateFigis
  case M.lookup myTicker figis of
    Just ti -> pure ti
    Nothing -> pure $ T.pack( "Ticker: " ++ (T.unpack myTicker) )

figiToLastPrice ∷ T.Text -> IO (Maybe Int)
figiToLastPrice myFigi = do
  prices <- readIORef statePrices
  case M.lookup myFigi prices of
    Just ps -> pure $ Just ps
    Nothing -> pure Nothing
