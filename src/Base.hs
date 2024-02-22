module Base
  ( module Export
  , cnyFigi
  , dollarFigi
  , runClient
  , sinceEpoch
  , toTimestamp
  ) where

import           Prelude.Unicode                        as Export

import           Data.ProtoLens.Message

import           Data.Int
import           Data.Time
import           Data.Time.Clock.POSIX

import           Client

import           Proto.Google.Protobuf.Timestamp
import qualified Proto.Google.Protobuf.Timestamp_Fields as TS

dollarFigi  ∷ String
cnyFigi     ∷ String

dollarFigi  = "BBG0013HGFT4"
cnyFigi     = "BBG0013HRTL0"

runClient ∷ ClientConfig -> IO GrpcClient
runClient cnfg = runExceptT (initGrpcClient cnfg) >>= \case
  Left err -> error ∘ show $ err
  Right gc -> pure gc

sinceEpoch ∷ UTCTime -> Int64
sinceEpoch = floor ∘ nominalDiffTimeToSeconds
                   ∘ utcTimeToPOSIXSeconds

toTimestamp ∷ Int64 -> Timestamp
toTimestamp s = build $ ( TS.seconds  .~ (s :: Int64) )
                      ∘ ( TS.nanos    .~ (0 :: Int32) )
