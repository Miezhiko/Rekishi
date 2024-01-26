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

runTicker ∷ GrpcClient -> String -> IO ()
runTicker g ticker = do
  let tickerText  = T.pack ticker
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
```

```bash
checking: TCSG, FIGI: BBG00QPYJ5H0
2024-01-19 07:00:00 UTC: 3108 rub
2024-01-22 07:00:00 UTC: 3133 rub
2024-01-23 07:00:00 UTC: 3107 rub
2024-01-24 07:00:00 UTC: 3077 rub
2024-01-25 07:00:00 UTC: 3069 rub
2024-01-26 07:00:00 UTC: 3072 rub
```
