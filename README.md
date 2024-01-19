```haskell
for_ (pf ^. O.positions) $ \pos -> do
  ticker <- figiToTicker $ pos ^. O.figi
  putStrLn $ "F: " ++ T.unpack ( ticker )
        ++ "\tQ: " ++ show ( pos ^. O.quantity ^. C.units )
        ++ "\tP: " ++ show ( pos ^. O.currentPrice ^. C.units )
```
