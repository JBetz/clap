module Clap.Library.Windows where

import Clap.Interface.Entry
import Foreign.Ptr
import System.Environment
import System.Win32.DLL as DLL
import System.Win32.Types

pluginLibraryPaths :: IO [FilePath]
pluginLibraryPaths = do
    commonProgramFiles <- getEnv "COMMONPROGRAMFILES"
    localAppData <- getEnv "LOCALAPPDATA" 
    pure [ commonProgramFiles <> "\\CLAP\\", localAppData <> "\\Programs\\Common\\CLAP\\" ]

newtype PluginLibrary = PluginLibrary { unPluginLibrary :: HMODULE }
    deriving (Show)

openPluginLibrary :: FilePath -> IO PluginLibrary
openPluginLibrary filePath = do
    library <- DLL.loadLibrary filePath
    pure $ PluginLibrary library

closePluginLibrary :: PluginLibrary -> IO ()
closePluginLibrary (PluginLibrary hmodule) =
    freeLibrary hmodule

lookupPluginEntry :: PluginLibrary -> IO PluginEntryHandle
lookupPluginEntry (PluginLibrary hmodule) = do
    addr <- DLL.getProcAddress hmodule "clap_entry"
    pure $ castPtr addr
