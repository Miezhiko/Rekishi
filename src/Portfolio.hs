module Portfolio
  ( runPortfolio
  ) where

import           Base
import           Figi
import           Ticker                  (getCandlesWith)
import           Types

import           Text.Printf

import           Data.Foldable           (for_)
import qualified Data.Map                as M
import           Data.Maybe              (catMaybes)
import           Data.ProtoLens.Message
import qualified Data.Text               as T
import           Data.Time

import           System.Console.ANSI

import           Client
import           Service.Operations      (getPortfolio)
import           Service.Users           (getAccounts)

import qualified Proto.Common_Fields     as C
import           Proto.Marketdata
import qualified Proto.Marketdata_Fields as MD
import           Proto.Operations
import qualified Proto.Operations_Fields as O
import           Proto.Users

runGetAccounts ∷ GrpcClient -> IO [Account]
runGetAccounts client = runExceptT (getAccounts client) >>= \case
  Left err -> error ∘ show $ err
  Right ac -> pure ac

-- GetBrokerReport GenerateBrokerReportRequest

runGetPortfolio ∷ GrpcClient -> PortfolioRequest -> IO PortfolioResponse
runGetPortfolio client pr =
  runExceptT (getPortfolio client pr) >>= \case
    Left err -> error ∘ show $ err
    Right pf -> pure pf

getPriceWithCandles ∷ [HistoricCandle] -> Float
getPriceWithCandles []    = 0.0
getPriceWithCandles [pos] =
  let myUnits = fromIntegral $ pos ^. MD.open ^. C.units
      myNanos = fromIntegral $ pos ^. MD.open ^. C.nano
      (cUn, cNan) = (myUnits, ( myNanos / 1000000000 :: Float ))
  in cUn + cNan
getPriceWithCandles (x:_) = getPriceWithCandles [x] -- should be single

getYesterdayPrice ∷ GrpcClient -> T.Text -> IO Float
getYesterdayPrice g myFigi = do
  now <- getCurrentTime
  let weekday     = dayOfWeek $ utctDay now
      unixTime    = sinceEpoch now
      day2Secs    = case weekday of
                     Monday -> 60 * 60 * 24 * 4
                     Sunday -> 60 * 60 * 24 * 3
                     _      -> 60 * 60 * 24 * 2
      day1Secs    = 60 * 60 * 24 * 1
      tfrom       = toTimestamp $ unixTime - day2Secs
      tto         = toTimestamp $ unixTime - day1Secs
  candles <- getCandlesWith tfrom tto g myFigi
  pure $ getPriceWithCandles candles

getAccountStuff ∷ GrpcClient -> [Account] -> IO ()
getAccountStuff _ []    = putStrLn "no accounts found for config"
getAccountStuff g [acc] = do
  let accId = acc ^. O.id
      pr    = build $ O.accountId .~ accId
  pf <- runGetPortfolio g pr
  dollarPriceMb <- figiToLastPrice $ T.pack dollarFigi
  cnyPriceMb    <- figiToLastPrice $ T.pack cnyFigi
  let dollarPrice = case dollarPriceMb of
                    Just (u, n) -> (fromIntegral u) + n
                    Nothing     -> 92.0 :: Float -- fair?
      cnyPrice    = case cnyPriceMb of
                      Just (u, n) -> (fromIntegral u) + n
                      Nothing     -> 13.0 :: Float -- fair?
      convPr c pp = case c of
                      "usd" -> pp * dollarPrice
                      "cny" -> pp * cnyPrice
                      _     -> pp

  reSharesMb <- traverse (\pos -> do
    let myFigi = pos ^. O.figi
        iType  = pos ^. O.instrumentType
    reShare <- figiToReShare myFigi
    lsPrice <- figiToLastPrice myFigi
    yPice   <- if (T.unpack iType) == "bond"
                  then pure 0
                  else getYesterdayPrice g myFigi
    case reShare of
      Just re -> do
        let realPrice =
              case lsPrice of
                Just (сUn, cNan) ->(fromIntegral сUn) + cNan
                Nothing ->
                  let myUnits = fromIntegral $ pos ^. O.currentPrice ^. C.units
                      myNanos = fromIntegral $ pos ^. O.currentPrice ^. C.nano
                  in myUnits + ( myNanos / 1000000000 :: Float )
            quantity  = fromIntegral $ pos ^. O.quantity ^. C.units :: Int
        pure $ Just (re, realPrice, yPice, quantity)
      Nothing -> pure Nothing) $ pf ^. O.positions

  let reShares = catMaybes reSharesMb
      (total, dtotal, prices) = foldl (\(summ, summd, dp) (re, realPrice, yPice, quantity) ->
                  let sCurrency = T.unpack $ currency re
                      rubPrice  = convPr sCurrency realPrice
                      rubyPrice = convPr sCurrency yPice
                      newToSumm = rubPrice * (fromIntegral quantity) :: Float
                      totalyPrice = rubyPrice * (fromIntegral quantity) :: Float
                      diffPrice   =
                        if totalyPrice == 0
                          then 0
                          else newToSumm - totalyPrice
                  in ( summ + newToSumm 
                     , summd + diffPrice
                     , (M.insert (figi re)
                                 (newToSumm, diffPrice) dp) ) )
                      ( 0.0, 0.0
                      , M.empty :: M.Map T.Text (Float, Float) ) reShares
      maxQl = maximum $ map (\(_, _, _, q) -> length (show q)) reShares
      maxOl = maximum $ map (\(_, _, y, _) -> length (printf "%.2f" y :: String)) reShares
      maxRl = maximum $ map (\(_, r, _, _) -> length (printf "%.2f" r :: String)) reShares
      maxTl = maximum $ map (\(_, (t, _))  -> length (show ( round t :: Int ))) $ M.toList prices
      maxDl = maximum $ map (\(_, (_, d))  -> length (show ( round d :: Int ))) $ M.toList prices

  for_ reShares $ \(re, realPrice, yPice, quantity) -> do
    let sCurrency = T.unpack $ currency re
        quantityS = show quantity
        quantityP = maxQl - length quantityS
        quantityA = concat $ replicate quantityP " "

        oldpS = if yPice == 0
                  then "-"
                  else printf "%.2f" yPice
        oldpP = maxOl - length oldpS
        oldpA = concat $ replicate oldpP " "

        newpS = if realPrice == 0
                  then "-"
                  else printf "%.2f" realPrice
        newpP = maxRl - length newpS
        newpA = concat $ replicate newpP " "

        (totalPrice, diffPrice) =
          case M.lookup (figi re) prices of
            Just (n, d) -> (n, d)
            Nothing     -> (0, 0)

        aprsS = show ( round totalPrice :: Int )
        aprsP = maxTl - length aprsS
        aprsA = concat $ replicate aprsP " "

        diffI = round diffPrice :: Int
        diffS = if diffI >= 0
                  then "+" ++ show diffI
                  else show diffI
        diffP = (maxDl + 1) - length diffS
        diffA = concat $ replicate diffP " "

    setSGR [ SetColor Foreground Vivid Cyan
           , SetConsoleIntensity BoldIntensity ]
    putStr $ T.unpack (figi re) --take 4 ( T.unpack ( ticker re ) )
    setSGR [ Reset ]
    putStr $ " " ++ quantityS ++ quantityA
          ++ " " ++ oldpS ++ oldpA
          ++ " -> " ++ newpS ++ " " ++ sCurrency ++ newpA
    setSGR [ SetColor Foreground Vivid White
           , SetConsoleIntensity BoldIntensity ]
    putStr $ " [ " ++ aprsS ++ " " ++ aprsA
    if diffPrice >= 0
      then do
        setSGR [ SetColor Foreground Vivid Green
               , SetConsoleIntensity BoldIntensity ]
      else do
        setSGR [ SetColor Foreground Vivid Red
               , SetConsoleIntensity BoldIntensity ]
    putStr $ diffS ++ diffA
    setSGR [ SetColor Foreground Vivid White
           , SetConsoleIntensity BoldIntensity ]
    putStr "] "
    setSGR [ Reset ]
    putStrLn $ T.unpack ( name re )
  putStrLn $ "Total Cap: " ++ show total
  putStr "Today: "
  if dtotal >= 0
      then do
        setSGR [ SetColor Foreground Vivid Green
               , SetConsoleIntensity BoldIntensity ]
        putStrLn $ "+" ++ printf "%.2f" dtotal
      else do
        setSGR [ SetColor Foreground Vivid Red
               , SetConsoleIntensity BoldIntensity ]
        putStrLn $ printf "%.2f" dtotal
  setSGR [ Reset ]
getAccountStuff g (x:_) = getAccountStuff g [x]

runPortfolio ∷ GrpcClient -> IO ()
runPortfolio client =
  runGetAccounts client
    >>= getAccountStuff client
