module Types
  ( module Export
  , ReMoneyValue (..)
  , ReQuotation (..)
  , ReShare (..)
  , SharesState (..)
  ) where

import           GHC.Generics (Generic)

import           Data.Binary
import           Data.Int     as Export
import           Data.IORef   as Export
import qualified Data.Map     as M
import qualified Data.Text    as T

data ReShare
  = ReShare
      { figi                  :: T.Text
      , ticker                :: T.Text
      , lot                   :: Int
      , currency              :: T.Text
      , dlong                 :: ReQuotation
      , dshort                :: ReQuotation
      , minPriceIncrement     :: ReQuotation
      , nominal               :: ReMoneyValue
      , name                  :: T.Text
      , apiTradeAvailableFlag :: Bool
      }
  deriving (Generic, Show)

data ReMoneyValue
  = ReMoneyValue
      { mCurrency :: T.Text
      , mUnits    :: Int
      , mNano     :: Int
      }
  deriving (Generic, Show)

data ReQuotation
  = ReQuotation
      { qUnits :: Int
      , qNano  :: Int
      }
  deriving (Generic, Show)

data SharesState
  = SharesState
      { fstateShares  :: [ReShare]
      , fstateTickers :: M.Map T.Text ReShare
      , fstateFigis   :: M.Map T.Text T.Text
      , fstatePrices  :: M.Map T.Text (Int, Float)
      }
  deriving (Generic, Show)

instance Binary ReShare
instance Binary ReMoneyValue
instance Binary ReQuotation
instance Binary SharesState
