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


withPluginLibrary :: FilePath -> (PluginLibrary -> IO ()) -> IO ()
withPluginLibrary filePath f = do
    pluginLibrary <- openPluginLibrary filePath
    f pluginLibrary
    closePluginLibrary pluginLibrary

scanForPlugins :: IO [PluginDescriptor]
scanForPlugins = do
    paths <- pluginLibraryPaths
    scanForPluginsIn paths

scanForPluginsIn :: [FilePath] -> IO [PluginDescriptor]
scanForPluginsIn directories = do 
    paths <- traverse (\directory -> do 
        exists <- doesPathExist directory
        if exists 
            then do
                paths <- findFilesWithExtension "clap" directory
                pure $ (\path -> directory </> path) <$> paths
            else pure []
        ) directories
    descriptors <- for (join paths) $ \filePath -> do
        library <- openPluginLibrary filePath
        entry <- lookupPluginEntry library
        maybeFactory <- getFactory entry pluginFactoryId
        case maybeFactory of
            Just factory -> getPluginDescriptor factory 0
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