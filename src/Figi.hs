{-# LANGUAGE
    FlexibleContexts
  , KindSignatures
  , RankNTypes
  #-}

module Figi
  ( figiToLastPrice
  , figiToReShare
  , loadBaseShares
  , restoreCache
  , storeCache
  , tickerToFigi
  ) where

import           Base
import           Console
import           State
import           Types

import           Data.Binary
import qualified Data.ByteString.Lazy     as BL
import qualified Data.Map                 as M
import qualified Data.ProtoLens.Field     as F
import           Data.ProtoLens.Message
import qualified Data.Text                as T

import           System.IO

import           Client
import           Service.Instruments      (bonds, currencies, etfs, futures, shares)
import           Service.MarketData       (getLastPrices)

import           Proto.Common
import qualified Proto.Common_Fields      as C
import           Proto.Instruments
import qualified Proto.Instruments_Fields as I
import           Proto.Marketdata
import qualified Proto.Marketdata_Fields  as MD

getBaseShares ∷ GrpcClient -> GrpcIO ([Share], [Currency], [Bond], [Future], [Etf])
getBaseShares gc = do
  myShares      <- shares     gc (defMessage & I.instrumentStatus .~ INSTRUMENT_STATUS_BASE)
  myCurrencies  <- currencies gc (defMessage & I.instrumentStatus .~ INSTRUMENT_STATUS_BASE)
  myBonds       <- bonds      gc (defMessage & I.instrumentStatus .~ INSTRUMENT_STATUS_BASE)
  myFutures     <- futures    gc (defMessage & I.instrumentStatus .~ INSTRUMENT_STATUS_BASE)
  myEtf         <- etfs       gc (defMessage & I.instrumentStatus .~ INSTRUMENT_STATUS_ALL)
  pure $ (myShares, myCurrencies, myBonds, myFutures, myEtf)

getReshare ∷ ∀ r. (
  (F.HasField r "figi" T.Text),
  (F.HasField r "ticker" T.Text),
  (F.HasField r "name" T.Text),
  (F.HasField r "lot" Int32),
  (F.HasField r "currency" T.Text),
  (F.HasField r "dlong" Quotation),
  (F.HasField r "dshort" Quotation),
  -- (F.HasField r "nominal" MoneyValue),
  (F.HasField r "apiTradeAvailableFlag" Bool)
  ) => r -> ReShare
getReshare s =
  let sLong     = s ^. I.dlong
      sShort    = s ^. I.dshort
      -- sNominal  = s ^. I.nominal
  in ReShare  ( s ^. I.figi                   )
              ( s ^. I.ticker                 )
              ( fromIntegral ( s ^. I.lot )   )
              ( s ^. I.currency               )
              ( ReQuotation ( fromIntegral ( sLong ^. C.units )     )
                            ( fromIntegral ( sLong ^. C.nano )      ) )
              ( ReQuotation ( fromIntegral ( sShort ^. C.units )    )
                            ( fromIntegral ( sShort ^. C.nano )     ) )
                            {-
              ( ReMoneyValue ( sNominal ^. C.currency               )
                             ( fromIntegral ( sNominal ^. C.units ) )
                             ( fromIntegral ( sNominal ^. C.nano )  ) )
                             -}
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
  -- (F.HasField r "nominal" MoneyValue),
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

loadBaseShares ∷ GrpcClient -> IO SharesState
loadBaseShares client = do
  progressThread <- startProgress "Loading base shares..."
  (s, c, b, f, e) <- runExceptT (getBaseShares client) >>= \case
    Left err -> error ∘ show $ err
    Right sh -> pure sh
  let (ss, stk, str) = getReshares s
      (cc, ctk, ctr) = getReshares c
      (bb, btk, btr) = getReshares b
      (ff, ftk, ftr) = getReshares f
      (ee, etk, etr) = getReshares e
      reShares       = ss ++ cc ++ bb ++ ff ++ ee 
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
  let priceMap = map (\p ->
                  let myPrice = p ^. MD.price
                      myUnits = fromIntegral $ myPrice ^. C.units
                      myNanos = fromIntegral $ myPrice ^. C.nano
                      untiNan = (myUnits, (( myNanos / 1000000000 ) :: Float))
                  in ( p ^. MD.figi, untiNan ) ) $ lastPricesS ++ lastPricesC
      mPriceMap = M.fromList priceMap
      mTickers  = M.fromList ( stk ++ ctk ++ btk ++ ftk ++ etk )
      mFigis    = M.fromList ( str ++ ctr ++ btr ++ ftr ++ etr )
  writeIORef statePrices mPriceMap 
  writeIORef stateTickers mTickers
  writeIORef stateFigis mFigis
  finishProgress progressThread
  pure $ SharesState reShares mTickers mFigis mPriceMap

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

figiToLastPrice ∷ T.Text -> IO (Maybe (Int, Float))
figiToLastPrice myFigi = do
  prices <- readIORef statePrices
  case M.lookup myFigi prices of
    Just ps -> pure $ Just ps
    Nothing -> pure Nothing

storeCache ∷ Handle -> SharesState -> IO ()
storeCache h = BL.hPut h ∘ encode

restoreCache ∷ GrpcClient -> Handle -> IO ()
restoreCache g h = BL.hGetContents h >>= \c -> do
  cache <-
    case decodeOrFail c of
      Left (_,_,err) -> do putStrLn $ "Failed to decode cache: " ++ err
                           loadBaseShares g
      Right (_,_,pc) -> pure pc
  writeIORef stateShares  $ fstateShares cache
  writeIORef statePrices  $ fstatePrices cache 
  writeIORef stateTickers $ fstateTickers cache
  writeIORef stateFigis   $ fstateFigis cache
