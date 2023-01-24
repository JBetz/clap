module Clap.Interface.Id where

import Data.Bits
import Data.Int

newtype ClapId = ClapId { unClapId :: Int }
    deriving (Show)

beatTimeFactor, secondsTimeFactor :: Int64
beatTimeFactor = 1 `shiftL` 31
secondsTimeFactor = 1 `shiftL` 31