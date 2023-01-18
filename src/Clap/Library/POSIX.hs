module Clap.Library.POSIX where

import Clap.Interface.Entry
import Foreign.Ptr
import System.Posix.DynamicLinker
import System.Directory

pluginLibraryPaths :: IO [FilePath]
pluginLibraryPaths = do
    appUserDataDirectory <- getAppUserDataDirectory "clap"
    pure [appUserDataDirectory, "/usr/lib/clap"]

newtype PluginLibrary = PluginLibrary { unPluginLibrary :: DL }
    deriving (Show)

openPluginLibrary :: FilePath -> IO PluginLibrary
openPluginLibrary path = PluginLibrary <$> dlopen path [RTLD_NOW]

closePluginLibrary :: PluginLibrary -> IO ()
closePluginLibrary (PluginLibrary dl) =
    dlclose dl

lookupPluginEntry :: PluginLibrary -> IO PluginEntryHandle
lookupPluginEntry (PluginLibrary dl) = do
    clapEntryFunPtr <- dlsym dl "clap_entry"
    pure $ castFunPtrToPtr clapEntryFunPtr
