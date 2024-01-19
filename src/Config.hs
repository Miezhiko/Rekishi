{-# LANGUAGE
    DeriveAnyClass
  , DeriveGeneric
  , DerivingStrategies
  #-}

module Config
  ( Config (..)
  , getCfg
  ) where

import           Data.Aeson
import qualified Data.Yaml    as Yaml

import           GHC.Generics

data Config
  = Config
      { cfgToken   :: String
      , cfgAppName :: String
      }
  deriving (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

getCfg ∷ IO Config
getCfg = Yaml.decodeFileThrow "conf.yml"
