module Clap 
    ( module Engine
    , module Extension
    , module Host
    , HostConfig (..)
    , ClapVersion (..)
    , hostClapVersion
    , scanForPlugins
    , pluginLibraryPaths
    , main
    ) where

import Clap.Engine as Engine
import Clap.Extension as Extension
import Clap.Host as Host
import qualified Clap.Interface.Extension.Gui as Gui
import Clap.Interface.Entry as Entry
import Clap.Interface.Host (HostConfig (..))
import Clap.Interface.Plugin as Plugin
import Clap.Interface.PluginFactory as PluginFactory
import Clap.Interface.Version as Version
import Clap.Interface.Foreign
import Clap.Library
import Control.Monad
import Control.Monad.Extra
import Data.Maybe
import Foreign.Ptr
import Foreign.Storable
import System.Directory
import System.FilePath

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
    
data Plugin = Plugin
    { plugin_handle :: PluginHandle
    , plugin_extensions :: Extensions
    }