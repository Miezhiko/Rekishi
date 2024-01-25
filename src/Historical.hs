module Historical
  ( runHistorical
  ) where

import           Base

import           Data.Foldable                  (for_)
import           Data.ProtoLens.Message
import qualified Data.Text                      as T

import           Invest.Client
import           Invest.Service.MarketData

import           Proto.Invest.Marketdata
import qualified Proto.Invest.Marketdata_Fields  as MD

runGetCandles ∷ GrpcClient -> GetCandlesRequest -> IO [HistoricCandle]
runGetCandles client gcr =
  runExceptT (getCandles client gcr) >>= \case
    Left err -> error ∘ show $ err
    Right cn -> pure cn

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

runHistorical ∷ GrpcClient -> IO ()
runHistorical g = do
  let Just daily = maybeToEnum 5
  let gcr = build $ ( MD.figi .~ (T.pack "XXX") )
                  . ( MD.interval .~ daily )
  cndls <- runGetCandles g gcr
  for_ cndls $ \pos -> do
    putStrLn $ show ( pos ^. MD.close )
