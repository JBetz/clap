module Clap.Interface.Foreign where

import Control.Applicative
import Data.Bits
import Data.Int
import Data.Maybe
import Data.Word
import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import System.Posix.DynamicLinker

fromCInt :: CInt -> Int
fromCInt = fromIntegral

fromCUInt :: CUInt -> Int
fromCUInt = fromIntegral

toCInt :: Int -> CInt
toCInt = fromIntegral

toCUInt :: Int -> CUInt
toCUInt = fromIntegral

fromCDouble :: CDouble -> Double
fromCDouble (CDouble double) = double

fromCChars :: [CChar] -> String
fromCChars = fmap castCCharToChar

pureIf :: (Alternative m) => Bool -> a -> m a
pureIf b a = if b then pure a else empty

newtype PluginLibrary = PluginLibrary { unPluginLibrary :: DL }
    deriving (Show)

withPluginLibrary :: FilePath -> (PluginLibrary -> IO ()) -> IO ()
withPluginLibrary filePath f = 
    withDL filePath [RTLD_NOW] (f . PluginLibrary)

flagsToInt :: Enum a => [a] -> Word32
flagsToInt flags = foldl1 (.|.) $ (\flag -> 1 `shiftL` fromEnum flag ) <$> flags

intToFlags :: (Enum a, Bounded a) => Word32 -> [a]
intToFlags int = 
    catMaybes $ (\flag -> if testBit int (fromEnum flag) then Just flag else Nothing) <$> [minBound .. maxBound]
