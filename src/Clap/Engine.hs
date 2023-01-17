module Clap.Engine where

import Clap.Interface.AudioBuffer
import Clap.Interface.Events
import Clap.Interface.Host
import Clap.Interface.Process
import Clap.Host as Host
import Control.Exception
import Data.Foldable (for_)
import Data.IORef
import Data.Int
import Data.List
import qualified Data.Map as Map
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import Sound.PortAudio as PortAudio
import Sound.PortAudio.Base

data Engine = Engine
    { engine_state :: IORef EngineState
    , engine_pluginHost :: PluginHost
    , engine_steadyTime :: Int64
    , engine_sampleRate :: Double
    , engine_numberOfFrames :: Int32
    , engine_inputs :: Ptr (Ptr CFloat)
    , engine_outputs :: Ptr (Ptr CFloat)
    , engine_audioStream :: IORef (Maybe (Stream CFloat CFloat))
    , engine_eventBuffer :: IORef [(EventConfig, Event)]
    }

data EngineState
    = StateStopped
    | StateRunning
    | StateStopping

createEngine :: HostConfig -> IO Engine
createEngine hostConfig = do
    state <- newIORef StateStopped
    pluginHost <- createPluginHost hostConfig
    inputs <- newArray [nullPtr, nullPtr]
    outputs <- newArray [nullPtr, nullPtr]
    audioStream <- newIORef Nothing
    eventBuffer <- newIORef []
    pure $ Engine
        { engine_state = state
        , engine_pluginHost = pluginHost
        , engine_steadyTime = 0
        , engine_sampleRate = 44100
        , engine_numberOfFrames = 0
        , engine_inputs = inputs   
        , engine_outputs = outputs
        , engine_audioStream = audioStream
        , engine_eventBuffer = eventBuffer
        }

start :: Engine -> IO (Maybe Error)
start engine = do
    initialize
    eitherStream <- openDefaultStream 2 0 (engine_sampleRate engine) (Just $ fromIntegral $ engine_numberOfFrames engine) (Just $ audioCallback engine) Nothing
    case eitherStream of
        Left portAudioError -> 
            pure $ Just portAudioError
        Right stream -> do
            writeIORef (engine_audioStream engine) (Just stream)
            let pluginHost = engine_pluginHost engine
            setPorts pluginHost (Data32 $ engine_inputs engine) (Data32 $ engine_outputs engine)
            activateAll pluginHost (engine_sampleRate engine) (engine_numberOfFrames engine)
            startStream stream
            pure Nothing

audioCallback :: Engine -> PaStreamCallbackTimeInfo -> [StreamCallbackFlag] -> CULong -> Ptr CFloat -> Ptr CFloat -> IO StreamResult
audioCallback engine timeInfo flags numberOfInputSamples inputPtr outputPtr = do
    let host = engine_pluginHost engine
    inputs <- peekArray (fromIntegral numberOfInputSamples) inputPtr
    let (left, right) = partition (\(i, _) -> even (i * 2)) (zip [0 ..] inputs)
    [leftBuffer, rightBuffer] <- peekArray 2 $ engine_inputs engine
    pokeArray leftBuffer (snd <$> left)
    pokeArray rightBuffer (snd <$> right)
    processBegin host (fromIntegral numberOfInputSamples) (engine_steadyTime engine)
    
    eventBuffer <- readIORef (engine_eventBuffer engine)
    for_ eventBuffer $ \(eventConfig, event) -> 
        processEvent host eventConfig event
    process host
        
    pure PortAudio.Continue 

stop :: Engine -> IO (Maybe Error)
stop engine = do
    maybeStream <- readIORef (engine_audioStream engine)
    case maybeStream of
        Just stream -> do
            stopStream stream
            closeStream stream
            terminate
        Nothing -> pure Nothing 

loadPlugin :: Engine -> PluginId -> IO ()
loadPlugin engine =
    Host.load (engine_pluginHost engine)

-- unloadPlugin :: Engine -> PluginId -> IO ()

-- isRunning :: Engine -> IO Bool