```haskell
for_ (pf ^. O.positions) $ \pos -> do
  ticker <- figiToTicker $ pos ^. O.figi
  putStrLn $ "F: " ++ T.unpack ( ticker )
        ++ "\tQ: " ++ show ( pos ^. O.quantity ^. C.units )
        ++ "\tP: " ++ show ( pos ^. O.currentPrice ^. C.units )
```

```haskell
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
```

```bash
{units: 10 nano: 520000000}
{units: 10 nano: 590000000}
{units: 10 nano: 590000000}
{units: 10 nano: 580000000}
{units: 10 nano: 550000000}
{units: 10 nano: 560000000}
```
