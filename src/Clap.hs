module Clap 
    ( module Entry
    , module Host
    -- , module Plugin
    , module PluginFactory
    , module Version
    , scanForPlugins
    , linuxPaths
    , loadPlugin 
    ) where

import Clap.Extension
import qualified Clap.Interface.Extension.Gui as Gui
import Clap.Interface.Entry as Entry
import Clap.Interface.Host as Host
import Clap.Interface.Plugin as Plugin
import Clap.Interface.PluginFactory as PluginFactory
import Clap.Interface.Version as Version
import Clap.Interface.Foreign
import Control.Monad
import Control.Monad.Extra
import Data.Maybe
import Foreign.Storable
import System.Directory
import System.Posix.DynamicLinker

scanForPlugins :: IO [FilePath]
scanForPlugins = do
    paths <- linuxPaths
    scanForPluginsIn paths

scanForPluginsIn :: [FilePath] -> IO [FilePath]
scanForPluginsIn directories = do 
    paths <- traverse (\directory -> do 
        exists <- doesPathExist directory
        if exists 
            then do
                paths <- listDirectory directory
                pure $ (\path -> directory <> "/" <> path) <$> paths
            else pure []
        ) directories
    pure $ join paths

linuxPaths :: IO [FilePath]
linuxPaths = do
    appUserDataDirectory <- getAppUserDataDirectory "clap"
    pure [appUserDataDirectory, "/usr/lib/clap"]

-- TODO
windowsPaths :: IO [FilePath]
windowsPaths = pure [ "%COMMONPROGRAMFILES%/CLAP/", "%LOCALAPPDATA%/Programs/Common/CLAP/" ]

loadPlugin :: HostHandle -> FilePath -> Int -> IO ()
loadPlugin host filePath pluginIndex =
    withPluginLibrary filePath $ \library -> do
        print library
        entry <- clapEntry library
        isInitialized <- Entry.init entry filePath
        when isInitialized $ do 
            maybeFactory <- getFactory entry clapPluginFactoryId
            whenJust maybeFactory $ \factory -> do 
                print factory
                count <- getPluginCount factory
                print count
                maybeDescriptor <- getPluginDescriptor factory pluginIndex
                whenJust maybeDescriptor $ \descriptor -> do
                    print descriptor
                    maybePlugin <- createPlugin factory host (pluginDescriptor_id descriptor) 
                    whenJust maybePlugin $ \plugin -> do 
                        print plugin
                        result <- Plugin.init plugin
                        print result
                        extensions <- initializeExtensions plugin
                        case extensions_gui extensions of
                            Just gui -> do
                                result <- Gui.isApiSupported gui plugin Gui.X11 True
                                putStrLn $ "isApiSupported: " <> show result
                            Nothing -> pure ()
                        Plugin.destroy plugin
            Entry.deinit entry
    
data Plugin = Plugin
    { plugin_handle :: PluginHandle
    , plugin_extensions :: Extensions
    }