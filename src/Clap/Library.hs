{-# LANGUAGE CPP #-}

module Clap.Library 
    ( PluginLibrary
    , pluginLibraryPaths
    , openPluginLibrary
    , closePluginLibrary
    , withPluginLibrary
    , lookupPluginEntry
    , scanForPlugins
    , scanForPluginsIn
    ) where

#ifdef WINDOWS
import Clap.Library.Windows
#else
import Clap.Library.POSIX
#endif
import Control.Monad
import System.Directory
import System.FilePath

withPluginLibrary :: FilePath -> (PluginLibrary -> IO ()) -> IO ()
withPluginLibrary filePath f = do
    pluginLibrary <- openPluginLibrary filePath
    f pluginLibrary
    closePluginLibrary pluginLibrary

scanForPlugins :: IO [FilePath]
scanForPlugins = do
    paths <- pluginLibraryPaths
    scanForPluginsIn paths

scanForPluginsIn :: [FilePath] -> IO [FilePath]
scanForPluginsIn directories = do 
    paths <- traverse (\directory -> do 
        exists <- doesPathExist directory
        if exists 
            then do
                paths <- listDirectory directory
                pure $ (\path -> directory </> path) <$> paths
            else pure []
        ) directories
    pure $ join paths