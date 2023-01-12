module Clap.Interface.PluginInvalidation where

import Clap.Interface.Foreign.PluginInvalidation
import Data.Word
import Foreign.C.String
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

newtype PluginInvalidationFactoryHandle = PluginInvalidationFactoryHandle { unPluginInvalidationFactoryHandle :: Ptr C'clap_plugin_invalidation_factory }
    deriving (Show)

data PluginInvalidationSource = PluginInvalidationSource
    { pluginInvalidationSource_directory :: String
    , pluginInvalidationSource_filenameGlob :: String
    , pluginInvalidationSource_recursiveScan :: Bool
    }

count :: PluginInvalidationFactoryHandle -> IO Word32
count (PluginInvalidationFactoryHandle pluginInvalidationFactory) = do
    funPtr <- peek $ p'clap_plugin_invalidation_factory'count pluginInvalidationFactory
    pure $ fromIntegral $ mK'count funPtr pluginInvalidationFactory

get :: PluginInvalidationFactoryHandle -> Word32 -> IO PluginInvalidationSource
get (PluginInvalidationFactoryHandle pluginInvalidationFactory) index = do
    funPtr <- peek $ p'clap_plugin_invalidation_factory'get pluginInvalidationFactory
    cPluginInvalidationSource <- peek $ mK'get funPtr pluginInvalidationFactory (fromIntegral index)
    directory <- peekCString $ c'clap_plugin_invalidation_source'directory cPluginInvalidationSource
    filenameGlob <- peekCString $ c'clap_plugin_invalidation_source'filename_glob cPluginInvalidationSource
    pure $ PluginInvalidationSource
        { pluginInvalidationSource_directory = directory
        , pluginInvalidationSource_filenameGlob = filenameGlob
        , pluginInvalidationSource_recursiveScan = toBool $ c'clap_plugin_invalidation_source'recursive_scan cPluginInvalidationSource
        }

refresh :: PluginInvalidationFactoryHandle -> IO Bool
refresh (PluginInvalidationFactoryHandle pluginInvalidationFactory) = do
    funPtr <- peek $ p'clap_plugin_invalidation_factory'refresh pluginInvalidationFactory
    pure $ toBool $ mK'refresh funPtr pluginInvalidationFactory
