module Clap.Interface.Foreign where

import Control.Applicative
import Data.Int
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

pureIf :: (Alternative m) => Bool -> a -> m a
pureIf b a = if b then pure a else empty

newtype PluginLibrary = PluginLibrary { unPluginLibrary :: DL }
    deriving (Show)

withPluginLibrary :: FilePath -> (PluginLibrary -> IO ()) -> IO ()
withPluginLibrary filePath f = 
    withDL filePath [RTLD_NOW] (f . PluginLibrary)

-- instance Storable String where
--     sizeOf clapVerson = #{size clap_plugin_descriptor_t}
--     alignment _ = #{alignment clap_plugin_descriptor_t}
--     peek pointer = cString <$> pointer
--     poke pointer string = 