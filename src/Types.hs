module Types
  ( ReMoneyValue (..)
  , ReShare (..)
  ) where

import qualified Data.Text as T

data ReShare
  = ReShare
      { figi                  :: T.Text
      , ticker                :: T.Text
      , lot                   :: Int
      , currency              :: T.Text
      , name                  :: T.Text
      -- , sector                :: T.Text
      -- , nominal               :: ReMoneyValue
      , apiTradeAvailableFlag :: Bool
      }
  deriving (Show)

data ReMoneyValue
  = ReMoneyValue
      { reCurrency :: T.Text
      , reUnits    :: Integer
      , reNano     :: Integer
      }
  deriving (Show)

{-
data ReQuotation
  = ReQuotation
      { reUnits :: Integer
      , reNano  :: Integer
      }
  deriving (Show)
-}
