{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Clap.Interface.Id where

import Data.Aeson
import Data.Bits
import Data.Int
import GHC.Generics

newtype ClapId = ClapId { unClapId :: Int }
    deriving (Show, Generic, ToJSON, FromJSON)

beatTimeFactor, secondsTimeFactor :: Int64
beatTimeFactor = 1 `shiftL` 31
secondsTimeFactor = 1 `shiftL` 31