module Clap.Interface.Fixedpoint where

import Data.Bits
import Data.Int
import Data.Word

beatTimeFactor, secondsTimeFactor :: Int64
beatTimeFactor = 1 `shiftL` 31
secondsTimeFactor =  1 `shiftL` 31

type BeatTime = Word64
type SecondsTime = Word64