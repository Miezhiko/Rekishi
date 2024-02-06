module State
  ( module Export
  , stateFigis
  , statePrices
  , stateShares
  , stateTickers
  ) where

import           Types

import           System.IO.Unsafe

import           Data.Int         as Export
import           Data.IORef       as Export
import qualified Data.Map         as M
import qualified Data.Text        as T

stateShares ∷ IORef [ReShare]
{-# NOINLINE stateShares #-}
stateShares = unsafePerformIO      $ newIORef []

stateTickers ∷ IORef (M.Map T.Text ReShare)
{-# NOINLINE stateTickers #-}
stateTickers = unsafePerformIO     $ newIORef M.empty

stateFigis ∷ IORef (M.Map T.Text T.Text)
{-# NOINLINE stateFigis #-}
stateFigis = unsafePerformIO       $ newIORef M.empty

statePrices ∷ IORef (M.Map T.Text Int)
{-# NOINLINE statePrices #-}
statePrices = unsafePerformIO      $ newIORef M.empty
