module Main where

import Clap.Engine as Engine
import Clap.Interface.Events
import Clap.Interface.Host (defaultHostConfig)
import Control.Concurrent

playNote :: Int -> IO ()
playNote duration = do
  engine <- createEngine defaultHostConfig
  let pluginId = ("plugins\\clap-saw-demo.clap", 0)
  loadPlugin engine pluginId
  startResult <- start engine
  print startResult
  putStrLn "noteOn"
  pushEvent engine 
    (EventConfig { eventConfig_time = 0, eventConfig_spaceId = 0, eventConfig_flags = []})
    (NoteOn (NoteEvent { noteEvent_noteId = 0, noteEvent_portIndex = 0, noteEvent_channel = 0, noteEvent_key = 80, noteEvent_velocity = 80 }))
  threadDelay $ duration * 1000
  putStrLn "noteOff"
  pushEvent engine 
    (EventConfig { eventConfig_time = 0, eventConfig_spaceId = 0, eventConfig_flags = []})
    (NoteOff (NoteEvent { noteEvent_noteId = 0, noteEvent_portIndex = 0, noteEvent_channel = 0, noteEvent_key = 80, noteEvent_velocity = 80 }))
  threadDelay $ 100 * 1000
  stopResult <- stop engine
  print stopResult