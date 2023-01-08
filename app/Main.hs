module Main where

import Clap

import Foreign.Marshal.Utils
import Foreign.Ptr

main :: IO ()
main = do 
  host <- createHost $ HostConfig
    { hostConfig_clapVersion = clapVersion
    , hostConfig_data = ()
    , hostConfig_name = "clap demo host"
    , hostConfig_vendor = "JBetz"
    , hostConfig_url = "github.com/JBetz/clap"
    , hostConfig_version = "0.1"
    , hostConfig_getExtension = \h s -> nullPtr
    , hostConfig_requestRestart = \h -> pure ()
    , hostConfig_requestProcess = \h -> pure ()
    , hostConfig_requestCallback = \h -> pure ()
    }
  plugins <- scanForPlugins
  print plugins
  loadPlugin host "plugins/clap-saw-demo.clap" 0