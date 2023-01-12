module Clap.Interface.Id where

import Clap.Interface.Foreign.Id
import Data.Bits
import Data.Int

newtype ClapId = ClapId { unClapId :: Int }
    deriving (Show)

clapBeattimeFactor, clapSectimeFactor :: Int64
clapBeattimeFactor = 1 `shiftL` 31
clapSectimeFactor = 1 `shiftL` 31