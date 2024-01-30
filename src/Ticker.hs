module Ticker
  ( runTicker
  ) where

import           Base
import           Figi

import           Data.Foldable                          (for_)
import           Data.Int
import           Data.ProtoLens.Message
import qualified Data.Text                              as T
import           Data.Time
import           Data.Time.Clock.POSIX

import           Invest.Client
import           Invest.Service.MarketData

import           Proto.Google.Protobuf.Timestamp
import qualified Proto.Google.Protobuf.Timestamp_Fields as TS
import qualified Proto.Invest.Common_Fields             as C
import           Proto.Invest.Marketdata
import qualified Proto.Invest.Marketdata_Fields         as MD

runGetCandles ∷ GrpcClient -> GetCandlesRequest -> IO [HistoricCandle]
runGetCandles client gcr =
  runExceptT (getCandles client gcr) >>= \case
    Left err -> error ∘ show $ err
    Right cn -> pure cn

sinceEpoch ∷ UTCTime -> Int64
sinceEpoch = floor ∘ nominalDiffTimeToSeconds
                   ∘ utcTimeToPOSIXSeconds

toTimestamp ∷ Int64 -> Timestamp
toTimestamp s = build $ ( TS.seconds  .~ (s :: Int64) )
                      ∘ ( TS.nanos    .~ (0 :: Int32) )

getCandlesWith ∷ Timestamp -- from
              -> Timestamp -- to
              -> GrpcClient
              -> T.Text 
              -> IO [HistoricCandle]
getCandlesWith tfrom tto g figi = do
  let Just daily  = maybeToEnum 5
  let gcr = build $ ( MD.figi     .~ figi )
                  ∘ ( MD.from     .~ tfrom )
                  ∘ ( MD.to       .~ tto )
                  ∘ ( MD.interval .~ daily )
  runGetCandles g gcr

getCandlesByTickerWith ∷ Timestamp -- from
                      -> Timestamp -- to
                      -> GrpcClient
                      -> String 
                      -> IO ([HistoricCandle], Int64, T.Text)
getCandlesByTickerWith tfrom tto g ticker = do
  let tickerText = T.pack ticker
  (lot, curr) <- tickerToLot tickerText
  figi        <- tickerToFigi tickerText
  candles     <- getCandlesWith tfrom tto g figi
  pure        (candles, fromIntegral lot, curr)

runTickerWith ∷ Timestamp -- from
             -> Timestamp -- to
             -> GrpcClient
             -> String 
             -> IO ()
runTickerWith tfrom tto g ticker = do
  (cndls, lot64, curr) <- getCandlesByTickerWith tfrom tto g ticker
  for_ cndls $ \pos ->
    let closePrice  = pos ^. MD.close ^. C.units * lot64
        timeSeconds = pos ^. MD.time ^. TS.seconds
        time        = posixSecondsToUTCTime (fromIntegral timeSeconds)
    in putStrLn $ show (utctDay time) ++ ": "
               ++ show closePrice ++ " " ++ (T.unpack curr)

runTicker ∷ GrpcClient -> String -> IO ()
runTicker g ticker = do
  now <- getCurrentTime
  let unixTime    = sinceEpoch now
      weekSecs    = 60 * 60 * 24 * 7
      tfrom       = toTimestamp $ unixTime - weekSecs
      tto         = toTimestamp unixTime
  runTickerWith tfrom tto g ticker
