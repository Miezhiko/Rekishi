```haskell
for_ (pf ^. O.positions) $ \pos -> do
  ticker <- figiToTicker $ pos ^. O.figi
  putStrLn $ "F: " ++ T.unpack ( ticker )
        ++ "\tQ: " ++ show ( pos ^. O.quantity ^. C.units )
        ++ "\tP: " ++ show ( pos ^. O.currentPrice ^. C.units )
```

```haskell
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
  figi <- tickerToFigi (T.pack ticker)
  putStrLn $ "checking: " ++ ticker ++ ", FIGI: " ++ (T.unpack figi)
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
  for_ cndls $ \pos -> do
    putStrLn $ show ( pos ^. MD.close )

```

```bash
./rekishi -t TCSG
checking: TCSG FIGI: BBG00QPYJ5H0
{units: 3108 nano: 500000000}
{units: 3133}
{units: 3107 nano: 500000000}
{units: 3077}
{units: 3069 nano: 500000000}
{units: 3090}
```
