module Main where

import Clap

import Foreign.Marshal.Utils
import Foreign.Ptr

main :: IO ()
main = do 
  engine <- createEngine $ HostConfig
    { hostConfig_clapVersion = hostClapVersion
    , hostConfig_data = nullPtr
    , hostConfig_name = "clap demo host"
    , hostConfig_vendor = "JBetz"
    , hostConfig_url = "github.com/JBetz/clap-hs"
    , hostConfig_version = "0.1"
    , hostConfig_getExtension = \h s -> pure nullPtr
    , hostConfig_requestRestart = \h -> pure ()
    , hostConfig_requestProcess = \h -> pure ()
    , hostConfig_requestCallback = \h -> pure ()
    }
  plugins <- scanForPlugins
  print plugins
  let pluginId = ("plugins/clap-saw-demo.clap", 0)
  loadPlugin engine pluginId
  startResult <- start engine
  print startResult
  stopResult <- stop engine
  print stopResult
  pure ()