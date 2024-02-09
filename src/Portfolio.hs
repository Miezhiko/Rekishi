module Portfolio
  ( runPortfolio
  ) where

import           Base
import           Figi
import           Ticker                         (getCandlesWith)
import           Types

import           Text.Printf

import           Data.Foldable                  (for_)
import           Data.Maybe                     (catMaybes)
import           Data.ProtoLens.Message
import qualified Data.Text                      as T
import           Data.Time

import           System.Console.ANSI

import           Invest.Client
import           Invest.Service.Operations      (getPortfolio)
import           Invest.Service.Users           (getAccounts)

import qualified Proto.Invest.Common_Fields     as C
import           Proto.Invest.Marketdata
import qualified Proto.Invest.Marketdata_Fields as MD
import           Proto.Invest.Operations
import qualified Proto.Invest.Operations_Fields as O
import           Proto.Invest.Users

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
  let unixTime    = sinceEpoch now
      day2Secs    = 60 * 60 * 24 * 2
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

  reSharesMb <- traverse (\pos -> do
    let myFigi = pos ^. O.figi
        iType  = pos ^. O.instrumentType
    reShare <- figiToReShare myFigi
    lsPrice <- figiToLastPrice myFigi
    yPice   <- getYesterdayPrice g myFigi
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
            yPrircCor = if (T.unpack iType) == "bond"
              then yPice * 10 -- I don't understand
              else yPice
        pure $ Just (re, realPrice, yPrircCor, quantity)
      Nothing -> pure Nothing) $ pf ^. O.positions

  dollarPriceMb <- figiToLastPrice $ T.pack dollarFigi
  let dollarPrice = case dollarPriceMb of
                      Just (u, n) -> (fromIntegral u) + n
                      Nothing     -> 90.0 :: Float -- fair?
      reShares = catMaybes reSharesMb
      (total, dtotal) = foldl (\(summ, summd) (re, realPrice, yPice, quantity) ->
                  let sCurrency = T.unpack $ currency re
                      rubPrice  = if sCurrency == "usd"
                                   then realPrice * dollarPrice
                                   else realPrice
                      rubyPrice = if sCurrency == "usd"
                                   then yPice * dollarPrice
                                   else yPice
                      newToSumm = rubPrice * (fromIntegral quantity) :: Float
                      totalyPrice = rubyPrice * (fromIntegral quantity) :: Float
                      diffPrice   =
                        if totalyPrice == 0
                          then 0
                          else newToSumm - totalyPrice
                  in ( summ + newToSumm 
                     , summd + diffPrice) ) (0.0, 0.0) reShares
      maxQl = maximum $ map (\(_, _, _, q) -> length (show q)) reShares

  for_ reShares $ \(re, realPrice, yPice, quantity) -> do
    let sCurrency = T.unpack $ currency re
        quantityS = show quantity
        quantityP = maxQl - length quantityS
        quantityA = concat $ replicate quantityP " "

        oldpS = if yPice == 0
                  then "NO TRADE"
                  else printf "%.2f" yPice
        oldpP = maxQl - length oldpS
        oldpA = concat $ replicate oldpP " "

        rubPrice  = if sCurrency == "usd"
            then realPrice * dollarPrice
            else realPrice
        totalPrice = rubPrice * (fromIntegral quantity)
        rubyPrice  = if sCurrency == "usd"
                      then yPice * dollarPrice
                      else yPice
        totalyPrice = rubyPrice * (fromIntegral quantity)
        diffPrice   =
          if totalyPrice == 0
            then 0
            else totalPrice - totalyPrice

    setSGR [ SetColor Foreground Vivid Cyan
           , SetConsoleIntensity BoldIntensity ]
    putStr $ take 4 ( T.unpack ( ticker re ) )
    setSGR [ Reset ]
    putStr $ "\tQ: " ++ quantityS ++ quantityA
          ++ "\tO: " ++ oldpS ++ oldpA
          ++ "\tP: " ++ printf "%.2f" ( realPrice ) ++ " " ++ sCurrency
    setSGR [ SetColor Foreground Vivid White
           , SetConsoleIntensity BoldIntensity ]
    putStr $ "\tA: " ++ show ( round totalPrice :: Int ) ++ " rub"
    if diffPrice >= 0
      then do
        setSGR [ SetColor Foreground Vivid Green
               , SetConsoleIntensity BoldIntensity ]
        putStr $ "\tD: +" ++ show ( round diffPrice :: Int )
      else do
        setSGR [ SetColor Foreground Vivid Red
               , SetConsoleIntensity BoldIntensity ]
        putStr $ "\tD: " ++ show ( round diffPrice :: Int )
    setSGR [ Reset ]
    putStrLn $ "\tN: " ++ T.unpack ( name re )
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
