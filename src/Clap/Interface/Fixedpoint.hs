module Clap.Interface.Fixedpoint where

import Clap.Interface.Foreign.Fixedpoint
import Data.Bits
import Data.Int
import Data.Word

-- TODO: Is this the correct type? Should it be Integer?
beatTimeFactor, secondsTimeFactor :: Int64
beatTimeFactor = 1 `shiftL` 31
secondsTimeFactor =  1 `shiftL` 31

type BeatTime = Word64
type SecondsTime = Word64