{-# LANGUAGE CPP #-}

module Clap.Library 
    ( PluginLibrary
    , pluginLibraryPaths
    , openPluginLibrary
    , closePluginLibrary
    , withPluginLibrary
    , lookupPluginEntry
    ) where

#ifdef WINDOWS
import Clap.Library.Windows
#else
import Clap.Library.POSIX
#endif

withPluginLibrary :: FilePath -> (PluginLibrary -> IO ()) -> IO ()
withPluginLibrary filePath f = do
    pluginLibrary <- openPluginLibrary filePath
    f pluginLibrary
    closePluginLibrary pluginLibrary