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

runTicker ∷ GrpcClient -> String -> IO ()
runTicker g ticker = do
  let tickerText = T.pack ticker
  (lot, curr) <- tickerToLot tickerText
  figi        <- tickerToFigi tickerText
  putStrLn $ "checking: " ++ ticker
          ++ ", FIGI: " ++ (T.unpack figi)
  now <- getCurrentTime
  let Just daily  = maybeToEnum 5
  let unixTime    = sinceEpoch now
      weekSecs    = 60 * 60 * 24 * 7
      tfrom       = toTimestamp $ unixTime - weekSecs
      tto         = toTimestamp unixTime
  let gcr = build $ ( MD.figi     .~ figi )
                  ∘ ( MD.from     .~ tfrom )
                  ∘ ( MD.to       .~ tto )
                  ∘ ( MD.interval .~ daily )
  cndls <- runGetCandles g gcr
  let lot64 = fromIntegral lot
  for_ cndls $ \pos ->
    let closePrice  = ( pos ^. MD.close ^. C.units ) * lot64
        timeSeconds = pos ^. MD.time ^. TS.seconds
        time        = posixSecondsToUTCTime (fromIntegral timeSeconds)
    in putStrLn $ show time ++ ": " ++ show closePrice ++ " " ++ (T.unpack curr)
