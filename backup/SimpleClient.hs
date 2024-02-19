module SimpleClient where

import           Config                   (Config (cfgToken))

import           Control.Monad            (void)

import           Client
import           Service.Instruments      (shares)
import           Service.MarketData

import           Proto.Instruments
import qualified Proto.Instruments_Fields as I
import           Proto.Marketdata
import qualified Proto.Marketdata_Fields  as MD

getBaseShares ∷ GrpcClient -> GrpcIO [Share]
getBaseShares gc =
  shares gc (defMessage & I.instrumentStatus .~ INSTRUMENT_STATUS_BASE)

getSharesLastPrices ∷ GrpcClient -> [Share] -> GrpcIO [LastPrice]
getSharesLastPrices gc myShares = toLastPrices (map (^. I.figi) myShares)
  where toLastPrices figis = getLastPrices gc (defMessage & MD.figi .~ figis)

-- Gets base shares prices and prints the first one
runSimpleClient ∷ Config -> IO ()
runSimpleClient cfg =
  void . runExceptT $ client <#> getBaseShares
                    #>> getSharesLastPrices #> (liftIO . print . head)
 where client = initGrpcClient $ ClientConfig {
                    token = (cfgToken cfg),
                    appName = Nothing
                  }
