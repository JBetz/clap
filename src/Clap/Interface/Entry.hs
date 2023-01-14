{-# LANGUAGE ScopedTypeVariables #-}

module Clap.Interface.Entry where

import Clap.Interface.Foreign.Entry
import Clap.Interface.PluginFactory
import Clap.Interface.Foreign
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import System.Posix.DynamicLinker

type PluginEntryHandle = Ptr C'clap_plugin_entry

clapEntry :: PluginLibrary -> IO PluginEntryHandle
clapEntry (PluginLibrary dl) = do
    clapEntryFunPtr <- dlsym dl "clap_entry"
    pure $ castFunPtrToPtr clapEntryFunPtr

init :: PluginEntryHandle -> FilePath -> IO Bool
init pluginEntry filePath = do
    funPtr <- peek $ p'clap_plugin_entry'init pluginEntry
    withCString filePath $ \cFilePath -> 
        pure $ toBool $ mK'init funPtr cFilePath
    
deinit :: PluginEntryHandle -> IO ()
deinit pluginEntry = do
    funPtr <- peek $ p'clap_plugin_entry'deinit pluginEntry
    mK'deinit funPtr

getFactory :: PluginEntryHandle -> String -> IO (Maybe PluginFactoryHandle)
getFactory pluginEntry pluginFactoryId = do
    funPtr <- peek $ p'clap_plugin_entry'get_factory pluginEntry
    withCString pluginFactoryId $ \cPluginFactoryId -> do
        let factory = mK'get_factory funPtr cPluginFactoryId
        pure $ if factory /= nullPtr
            then Just $ castPtr factory
            else Nothing
