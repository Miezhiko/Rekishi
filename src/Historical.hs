module Historical
  ( runHistorical
  ) where

import           Base

import           Data.Foldable                          (for_)
import           Data.Int
import           Data.ProtoLens.Message
import qualified Data.Text                              as T
import           Data.Time
import           Data.Time.Clock.POSIX

import           Invest.Client
import           Invest.Service.MarketData

import           Proto.Invest.Marketdata
import qualified Proto.Invest.Marketdata_Fields         as MD

import           Proto.Google.Protobuf.Timestamp
import qualified Proto.Google.Protobuf.Timestamp_Fields as TS

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

runHistorical ∷ GrpcClient -> IO ()
runHistorical g = do
  now <- getCurrentTime
  let Just daily  = maybeToEnum 5
  let unixTime    = sinceEpoch now
      weekSecs    = 60 * 60 * 24 * 7
      tfrom       = toTimestamp $ unixTime - weekSecs
      tto         = toTimestamp unixTime
  let gcr = build $ ( MD.figi     .~ (T.pack "TCS00A107563") )
                  ∘ ( MD.from     .~ tfrom )
                  ∘ ( MD.to       .~ tto )
                  ∘ ( MD.interval .~ daily )
  cndls <- runGetCandles g gcr
  for_ cndls $ \pos -> do
    putStrLn $ show ( pos ^. MD.close )

--message GetCandlesRequest {
--  string figi = 1;
--  google.protobuf.Timestamp from = 2;
--  google.protobuf.Timestamp to = 3;
--  CandleInterval interval = 4;
--}

-- CandleInterval enum
{-
  CANDLE_INTERVAL_UNSPECIFIED = 0; //???
  CANDLE_INTERVAL_1_MIN       = 1; //1min
  CANDLE_INTERVAL_5_MIN       = 2; //5min
  CANDLE_INTERVAL_15_MIN      = 3; //15min
  CANDLE_INTERVAL_HOUR        = 4; //hour
  CANDLE_INTERVAL_DAY         = 5; //day
-}

{-
message HistoricCandle {
  Quotation open = 1;   //Цена открытия за 1 инструмент. Для получения стоимости лота требуется умножить на лотность инструмента.
  Quotation high = 2;   //Максимальная цена за 1 инструмент. Для получения стоимости лота требуется умножить на лотность инструмента.
  Quotation low = 3;    //Минимальная цена за 1 инструмент. Для получения стоимости лота требуется умножить на лотность инструмента.
  Quotation close = 4;  //Цена закрытия за 1 инструмент. Для получения стоимости лота требуется умножить на лотность инструмента.
  int64 volume = 5;     //Объём торгов в лотах.
  google.protobuf.Timestamp time = 6; //Время свечи в часовом поясе UTC.
  bool is_complete = 7; //Признак завершённости свечи. **false** значит, свеча за текущие интервал ещё сформирована не полностью.
}
-}
