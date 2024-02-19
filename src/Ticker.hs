module Ticker
  ( getCandlesWith
  , runTicker
  ) where

import           Base
import           Figi
import           Types

import           Data.Foldable                          (for_)
import           Data.ProtoLens.Message
import qualified Data.Text                              as T
import           Data.Time
import           Data.Time.Clock.POSIX

import           Client
import           Service.MarketData

import qualified Proto.Common_Fields                    as C
import           Proto.Google.Protobuf.Timestamp
import qualified Proto.Google.Protobuf.Timestamp_Fields as TS
import           Proto.Marketdata
import qualified Proto.Marketdata_Fields                as MD

runGetCandles ∷ GrpcClient -> GetCandlesRequest -> IO [HistoricCandle]
runGetCandles client gcr =
  runExceptT (getCandles client gcr) >>= \case
    Left err -> error ∘ show $ err
    Right cn -> pure cn

getCandlesWith ∷ Timestamp -- from
              -> Timestamp -- to
              -> GrpcClient
              -> T.Text 
              -> IO [HistoricCandle]
getCandlesWith tfrom tto g myFigi = do
  let Just daily  = maybeToEnum 5
  let gcr = build $ ( MD.figi     .~ myFigi )
                  ∘ ( MD.from     .~ tfrom )
                  ∘ ( MD.to       .~ tto )
                  ∘ ( MD.interval .~ daily )
  runGetCandles g gcr

getCandlesByTickerWith ∷ Timestamp -- from
                      -> Timestamp -- to
                      -> GrpcClient
                      -> String 
                      -> IO (Maybe ([HistoricCandle], T.Text))
getCandlesByTickerWith tfrom tto g myTicker = do
  let tickerText = T.pack myTicker
  myFigi      <- tickerToFigi tickerText
  reShare     <- figiToReShare myFigi
  case reShare of
    Just re -> do
      candles  <- getCandlesWith tfrom tto g myFigi
      pure $ Just (candles, currency re)
    Nothing -> pure Nothing

runTickerWith ∷ Timestamp -- from
             -> Timestamp -- to
             -> GrpcClient
             -> String 
             -> IO ()
runTickerWith tfrom tto g myTicker = do
  maybeCandles <- getCandlesByTickerWith tfrom tto g myTicker
  case maybeCandles of
    Just (cndls, curr) ->
      for_ cndls $ \pos ->
        let myUnits     = fromIntegral $ pos ^. MD.close ^. C.units
            myNanos     = fromIntegral $ pos ^. MD.close ^. C.nano
            closePrice  = myUnits + ( myNanos / 1000000000 :: Float )
            timeSeconds = pos ^. MD.time ^. TS.seconds
            time        = posixSecondsToUTCTime (fromIntegral timeSeconds)
        in putStrLn $ show (utctDay time) ++ ": "
                   ++ show closePrice ++ " " ++ (T.unpack curr)
    Nothing -> putStrLn "No reshare found"

runTicker ∷ GrpcClient -> String -> IO ()
runTicker g myTicker = do
  now <- getCurrentTime
  let unixTime    = sinceEpoch now
      weekSecs    = 60 * 60 * 24 * 7
      tfrom       = toTimestamp $ unixTime - weekSecs
      tto         = toTimestamp unixTime
  runTickerWith tfrom tto g myTicker
