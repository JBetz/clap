{-# LANGUAGE CPP #-}

module Clap.Library 
    ( PluginLibrary
    , PluginInfo (..)
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
import Clap.Interface.Entry as Entry
import Clap.Interface.Plugin
import Clap.Interface.PluginFactory
import Control.Monad
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Traversable (for)
import System.Directory
import System.FilePath
import System.IO.Unsafe (unsafeInterleaveIO)

data PluginInfo = PluginInfo
    { pluginInfo_filePath :: FilePath
    , pluginInfo_index :: Int
    , pluginInfo_descriptor :: PluginDescriptor
    }

withPluginLibrary :: FilePath -> (PluginLibrary -> IO a) -> IO a
withPluginLibrary filePath f = do
    pluginLibrary <- openPluginLibrary filePath
    result <- f pluginLibrary
    closePluginLibrary pluginLibrary
    pure result

scanForPlugins :: IO [PluginInfo]
scanForPlugins = do
    paths <- pluginLibraryPaths
    scanForPluginsIn paths

scanForPluginsIn :: [FilePath] -> IO [PluginInfo]
scanForPluginsIn directories = do 
    paths <- traverse (\directory -> do 
        exists <- doesPathExist directory
        if exists 
            then do
                paths <- findFilesWithExtension "clap" directory
                pure $ (directory </>) <$> paths
            else pure []
        ) directories
    descriptors <- for (join paths) $ \filePath -> 
        withPluginLibrary filePath $ \library -> do 
            entry <- lookupPluginEntry library
            maybeFactory <- getFactory entry clapPluginFactoryId
            case maybeFactory of
                Just factory -> do
                    maybeDescriptor <- getPluginDescriptor factory 0
                    pure $ case maybeDescriptor of
                        Just descriptor -> Just $ PluginInfo
                            { pluginInfo_filePath = filePath
                            , pluginInfo_index = 0 
                            , pluginInfo_descriptor = descriptor
                            }
                        Nothing -> Nothing
                Nothing -> pure Nothing
    pure $ catMaybes descriptors

findFilesWithExtension :: String -> FilePath -> IO [FilePath]
findFilesWithExtension extension filePath = do
    allEntries <- listDirectory filePath
    let relativeEntries = mkRel <$> allEntries
    let matches = filter (\name -> ('.':extension) `isSuffixOf` name) relativeEntries
    dirs <- filterM doesDirectoryExist relativeEntries
    case dirs of
        [] -> pure matches
        ds -> do
            next <- unsafeInterleaveIO $ foldMapA (findFilesWithExtension extension) ds
            pure $ matches ++ next
    where mkRel = (filePath </>)
          foldMapA = (fmap fold .) . traverse