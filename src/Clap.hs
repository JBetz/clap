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
import Clap.Interface.Events
import Clap.Interface.Host (HostConfig (..))
import Clap.Interface.Plugin as Plugin
import Clap.Interface.PluginFactory as PluginFactory
import Clap.Interface.Version as Version
import Clap.Interface.Foreign
import Clap.Library
import Control.Concurrent
import Control.Monad
import Control.Monad.Extra
import Data.Maybe
import Data.Foldable (for_)
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
    }

main :: IO ()
main = do
  engine <- createEngine $ HostConfig
    { hostConfig_clapVersion = hostClapVersion
    , hostConfig_data = nullPtr
    , hostConfig_name = "clap demo host"
    , hostConfig_vendor = "JBetz"
    , hostConfig_url = "github.com/JBetz/clap-hs"
    , hostConfig_version = "0.1"
    , hostConfig_getExtension = \h s -> print (h, s) >> pure nullPtr
    , hostConfig_requestRestart = \h -> pure ()
    , hostConfig_requestProcess = \h -> pure ()
    , hostConfig_requestCallback = \h -> pure ()
    }
  let pluginId = ("C:\\Users\\joebe\\AppData\\Local\\Programs\\Common\\CLAP\\clap-saw-demo.clap", 0)
  loadPlugin engine pluginId
  startResult <- start engine
  print startResult
  pushEvent engine 
    (EventConfig { eventConfig_time = 0, eventConfig_spaceId = 0, eventConfig_flags = []})
    (NoteOn (NoteEvent { noteEvent_noteId = 0, noteEvent_portIndex = 0, noteEvent_channel = 0, noteEvent_key = 80, noteEvent_velocity = 80 }))
  threadDelay $ 1000 * 1000
  pushEvent engine 
    (EventConfig { eventConfig_time = 1, eventConfig_spaceId = 0, eventConfig_flags = []})
    (NoteOff (NoteEvent { noteEvent_noteId = 0, noteEvent_portIndex = 0, noteEvent_channel = 0, noteEvent_key = 80, noteEvent_velocity = 80 }))
  threadDelay $ 1000 * 1000
  stopResult <- stop engine
  print stopResult
  pure ()