module Types
  ( ReMoneyValue (..)
  , ReQuotation (..)
  , ReShare (..)
  ) where

import qualified Data.Text as T

data ReShare
  = ReShare
      { figi                  :: T.Text
      , ticker                :: T.Text
      , lot                   :: Int
      , currency              :: T.Text
      , dlong                 :: ReQuotation
      , dshort                :: ReQuotation
      -- nominal feels likecompletely useless value
      -- might drop later
      , nominal               :: ReMoneyValue
      , name                  :: T.Text
      , apiTradeAvailableFlag :: Bool
      }
  deriving (Show)

data ReMoneyValue
  = ReMoneyValue
      { mCurrency :: T.Text
      , mUnits    :: Int
      , mNano     :: Int
      }
  deriving (Show)

data ReQuotation
  = ReQuotation
      { qUnits :: Int
      , qNano  :: Int
      }
  deriving (Show)
