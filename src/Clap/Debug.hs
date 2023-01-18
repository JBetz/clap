module Clap.Debug where

import Clap.Engine as Engine
import Clap.Host as Host
import Clap.Interface.Host (HostConfig (..))
import Clap.Interface.Version as Version
import Foreign.Ptr

reproduceFailure :: IO ()
reproduceFailure = do
  engine <- createEngine $ HostConfig
    { hostConfig_clapVersion = hostClapVersion
    , hostConfig_data = nullPtr
    , hostConfig_name = "clap demo host"
    , hostConfig_vendor = "JBetz"
    , hostConfig_url = "github.com/JBetz/clap-hs"
    , hostConfig_version = "0.1"
    , hostConfig_getExtension = \_h _s -> pure nullPtr
    , hostConfig_requestRestart = \_h -> pure ()
    , hostConfig_requestProcess = \_h -> pure ()
    , hostConfig_requestCallback = \_h -> pure ()
    }
  let pluginId = ("C:\\Users\\joebe\\Projects\\clap-hs\\plugins\\clap-saw-demo.clap", 0)
  loadPlugin engine pluginId
  result <- Host.activate (engine_pluginHost engine) pluginId (engine_sampleRate engine) (engine_numberOfFrames engine)
  print result

reproduceSuccess :: IO ()
reproduceSuccess = undefined