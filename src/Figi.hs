{-# LANGUAGE
    FlexibleContexts
  , KindSignatures
  , RankNTypes
  #-}

module Figi
  ( figiToReShare
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
import           Invest.Service.Instruments      (bonds, currencies, etfs, futures, shares)

-- import qualified Proto.Invest.Common_Fields      as C
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

getReshare ∷ ∀ r. (
  (F.HasField r "figi" T.Text),
  (F.HasField r "ticker" T.Text),
  (F.HasField r "name" T.Text),
  (F.HasField r "lot" Int32),
  (F.HasField r "currency" T.Text),
  -- (F.HasField r "sector" T.Text),
  (F.HasField r "apiTradeAvailableFlag" Bool)
  ) => r -> ReShare
getReshare s =
  -- let sNominal = s ^. I.nominal
  --    nominal = 
  --      ReMoneyValue  ( sNominal ^. C.currency  )
  --                    ( sNominal ^. C.units     )
  --                    ( sNominal ^. C.nano      )
  -- in
     ReShare  ( s ^. I.figi                   )
              ( s ^. I.ticker                 )
              ( fromIntegral ( s ^. I.lot )   )
              ( s ^. I.currency               )
              ( s ^. I.name                   )
              -- ( s ^. I.sector                 )
              -- ( nominal                       )
              ( s ^. I.apiTradeAvailableFlag  )

getReshares ∷ ∀ r. (
  (F.HasField r "figi" T.Text),
  (F.HasField r "ticker" T.Text),
  (F.HasField r "name" T.Text),
  (F.HasField r "lot" Int32),
  (F.HasField r "currency" T.Text),
  -- (F.HasField r "sector" T.Text),
  (F.HasField r "apiTradeAvailableFlag" Bool)
  ) => [r] -> ([ReShare], [(T.Text, ReShare)], [(T.Text, T.Text)])
getReshares xs =
  let reShares    = map getReshare xs
      reSharesMap = map (\r -> (figi r, r)) reShares
      tickerFigi  = map (\r -> (ticker r, figi r)) reShares
  in (reShares, reSharesMap, tickerFigi)

loadBaseShares ∷ GrpcClient -> IO ()
loadBaseShares client = do
  (s, c, b, f, e) <- runExceptT (getBaseShares client) >>= \case
    Left err -> error ∘ show $ err
    Right sh -> pure sh
  let (ss, stk, str) = getReshares s
      (cc, ctk, ctr) = getReshares c
      (bb, btk, btr) = getReshares b
      (ff, ftk, ftr) = getReshares f
      (ee, etk, etr) = getReshares e
  writeIORef stateShares  $ ss ++ cc ++ bb ++ ff ++ ee 
  writeIORef stateTickers $ M.fromList ( stk ++ ctk ++ btk ++ ftk ++ etk )
  writeIORef stateFigis   $ M.fromList ( str ++ ctr ++ btr ++ ftr ++ etr )

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
