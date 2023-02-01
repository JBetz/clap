{-# LANGUAGE BangPatterns #-}

module Clap.Host where

import Clap.Extension
import Clap.Interface.AudioBuffer
import Clap.Interface.Entry as Entry
import Clap.Interface.Plugin (PluginHandle, PluginDescriptor (..))
import qualified Clap.Interface.Plugin as Plugin
import Clap.Interface.Events
import Clap.Interface.PluginFactory
import Clap.Interface.Process
import Clap.Interface.Host as Host
import Clap.Interface.Version
import Clap.Library
import Control.Exception
import Control.Monad
import Control.Monad.Extra
import Data.Foldable (for_)
import Data.Int
import Data.Word
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Traversable (for)
import Foreign.C.Types
import Foreign.Ptr

data ThreadType 
    = MainThread
    | AudioThread
    | Unknown
    deriving (Show)

data PluginHost = PluginHost
    { pluginHost_handle :: HostHandle
    , pluginHost_plugins :: IORef (Map PluginId Plugin)
    , pluginHost_threadType :: IORef ThreadType
    , pluginHost_extensions :: HostExtensions
    }

data PluginId = PluginId 
    { pluginId_filePath :: FilePath
    , pluginId_index ::  Int 
    } deriving (Eq, Ord, Show)

data PluginState
    = Inactive
    | InactiveWithError
    | ActiveAndSleeping
    | ActiveAndProcessing
    | ActiveWithError
    | ActiveAndReadyToDeactivate
    deriving (Eq, Show)

data Plugin = Plugin
    { plugin_library :: PluginLibrary
    , plugin_entry :: PluginEntryHandle
    , plugin_factory :: PluginFactoryHandle
    , plugin_descriptor :: PluginDescriptor
    , plugin_handle :: PluginHandle
    , plugin_state :: IORef PluginState
    , plugin_events :: IORef [EventHandle]
    , plugin_inputEvents :: InputEventsHandle
    , plugin_outputEvents :: OutputEventsHandle
    , plugin_processStatus :: IORef (Maybe ProcessStatus)
    , plugin_process :: ProcessHandle
    , plugin_audioIn :: AudioBufferHandle
    , plugin_audioOut :: AudioBufferHandle
    }

data PluginException
    = InvalidPath 
    | InvalidEntryPoint
    | InvalidIndex
    | NoDescriptor
    | IncompatibleClapVersion
    | EntryInitializationFailed
    | PluginInitializationFailed
    | CreationFailed
    | ActivationFailed
    | StartProcessingFailed
    | InvalidPluginId
    deriving (Show)

instance Exception PluginException

createPluginHost :: HostConfig -> IO PluginHost
createPluginHost hostConfig = do
    extensions <- initializeExtensions
    hostHandle <- Host.createHost $ hostConfig { hostConfig_getExtension = \_ name -> Clap.Extension.getExtension extensions name }
    plugins <- newIORef mempty
    threadType <- newIORef Unknown
    pure $ PluginHost
        { pluginHost_handle = hostHandle 
        , pluginHost_plugins = plugins
        , pluginHost_threadType = threadType
        , pluginHost_extensions = extensions
        }

load :: PluginHost -> PluginId -> IO ()
load host (PluginId filePath index) = do
    let hostHandle = pluginHost_handle host 
    library <- openPluginLibrary filePath
    entry <- lookupPluginEntry library
    isEntryInitialized <- Entry.init entry filePath
    unless isEntryInitialized $ throw EntryInitializationFailed
    maybeFactory <- getFactory entry clapPluginFactoryId
    whenJust maybeFactory $ \factory -> do
        count <- getPluginCount factory
        when (index > count) $ throw InvalidIndex
        maybeDescriptor <- getPluginDescriptor factory index
        case maybeDescriptor of
            Nothing -> throw NoDescriptor
            Just descriptor -> do
                unless (clapVersionIsCompatible $ pluginDescriptor_clapVersion descriptor) $ throw IncompatibleClapVersion
                maybePluginHandle <- createPlugin factory hostHandle (pluginDescriptor_id descriptor) 
                case maybePluginHandle of
                    Nothing -> throw CreationFailed
                    Just pluginHandle -> do 
                        isPluginInitialized <- Plugin.init pluginHandle
                        unless isPluginInitialized $ throw PluginInitializationFailed
                        state <- newIORef Inactive
                        processStatus <- newIORef Nothing
                        events <- newIORef []
                        inputEvents <- createInputEvents events
                        outputEvents <- createOutputEvents
                        process' <- createProcess
                        audioIn <- createAudioBuffer
                        audioOut <- createAudioBuffer
                        addPlugin (PluginId filePath index) $ Plugin
                            { plugin_library = library
                            , plugin_entry = entry
                            , plugin_factory = factory
                            , plugin_descriptor = descriptor
                            , plugin_handle = pluginHandle
                            , plugin_state = state
                            , plugin_processStatus = processStatus
                            , plugin_events = events
                            , plugin_inputEvents = inputEvents
                            , plugin_outputEvents = outputEvents
                            , plugin_process = process'
                            , plugin_audioIn = audioIn
                            , plugin_audioOut = audioOut
                            }
    where        
        addPlugin :: PluginId -> Plugin -> IO ()
        addPlugin key plugin =
            modifyIORef' (pluginHost_plugins host) $ Map.insert key plugin

activate :: Plugin -> Double -> Word32 -> IO ()
activate plugin sampleRate blockSize = do
    isActivated <- Plugin.activate (plugin_handle plugin) sampleRate blockSize blockSize
    setState plugin $ if isActivated 
        then ActiveAndSleeping
        else InactiveWithError 

activateAll :: PluginHost -> Double -> Word32 -> IO ()
activateAll host sampleRate blockSize =
    void $ forEachPlugin host $ \plugin -> 
        activate plugin sampleRate blockSize

deactivate :: Plugin -> IO ()
deactivate plugin =
    whenM (isPluginActive plugin) $ do 
        Plugin.deactivate (plugin_handle plugin)
        setState plugin Inactive

deactivateAll :: PluginHost -> IO ()
deactivateAll host = void $ forEachPlugin host deactivate

processAll :: PluginHost -> IO [PluginOutput]
processAll host = forEachPlugin host process

processBeginAll :: PluginHost -> Word64 -> Int64 -> IO ()
processBeginAll host framesCount steadyTime = do
    setThreadType host AudioThread
    plugins <- readIORef (pluginHost_plugins host)
    for_ (Map.elems plugins) $ \plugin -> processBegin plugin framesCount steadyTime

processBegin :: Plugin -> Word64 -> Int64 -> IO ()
processBegin plugin framesCount steadyTime = do
    let process' = plugin_process plugin
    setFramesCount process' framesCount
    setSteadyTime process' steadyTime

processEvent :: PluginHost -> PluginId -> EventConfig -> Event -> IO ()
processEvent host pluginId eventConfig event = do
    plugin <- getPlugin host pluginId
    push (plugin_events plugin) eventConfig event

process :: Plugin -> IO PluginOutput
process plugin = do
    let process' = plugin_process plugin
    setTransport process' nullPtr
    setInputEvents process' (plugin_inputEvents plugin)
    setOutputEvents process' (plugin_outputEvents plugin)
    setAudioInputs process' (plugin_audioIn plugin)
    setAudioInputsCount process' 1
    setAudioOutputs process' (plugin_audioOut plugin)
    setAudioOutputsCount process' 1
    whenM (isPluginSleeping plugin) $ do
        !isStarted <- Plugin.startProcessing (plugin_handle plugin)
        setState plugin $ if isStarted 
            then ActiveAndProcessing
            else ActiveWithError
    whenM (isPluginProcessing plugin) $ do
        !status <- Plugin.process (plugin_handle plugin) process'
        setProcessStatus plugin status
    getAudioOutput plugin

forEachPlugin :: PluginHost -> (Plugin -> IO a) -> IO [a]
forEachPlugin host f = do
    plugins <- readIORef $ pluginHost_plugins host
    for (Map.elems plugins) f

data PluginOutput = PluginOutput
    { pluginOutput_leftChannel :: [CFloat]
    , pluginOutput_rightChannel :: [CFloat] 
    }

getAudioOutput :: Plugin -> IO PluginOutput
getAudioOutput plugin = do
    let process' = plugin_process plugin    
    frameCount <- getFrameCount process'
    [leftChannel, rightChannel] <- getBufferData32 (plugin_audioOut plugin) frameCount
    pure $ PluginOutput
        { pluginOutput_leftChannel = leftChannel
        , pluginOutput_rightChannel = rightChannel
        }

processEnd :: Plugin -> Word64 -> Int64 -> IO ()
processEnd plugin numberOfFrames steadyTime = do
    let process' = plugin_process plugin
    setFramesCount process' numberOfFrames
    setSteadyTime process' steadyTime

setThreadType :: PluginHost -> ThreadType -> IO ()
setThreadType host =
    writeIORef (pluginHost_threadType host)

setPorts :: Plugin -> BufferData t -> BufferData t -> IO ()
setPorts plugin inputs outputs = do
    let audioIn = plugin_audioIn plugin
    setChannelCount audioIn 2
    setBufferData audioIn inputs
    setConstantMask audioIn 0
    setLatency audioIn 0
    
    let audioOut = plugin_audioOut plugin
    setChannelCount audioOut 2
    setBufferData audioOut outputs
    setConstantMask audioOut 0
    setLatency audioOut 0

getPlugin :: PluginHost -> PluginId -> IO Plugin
getPlugin host pluginId = do
    plugins <- readIORef $ pluginHost_plugins host
    case Map.lookup pluginId plugins of
        Nothing -> throw InvalidPluginId
        Just plugin -> pure plugin

setState :: Plugin -> PluginState -> IO ()
setState plugin = writeIORef (plugin_state plugin)

setProcessStatus :: Plugin -> ProcessStatus -> IO ()
setProcessStatus plugin = writeIORef (plugin_processStatus plugin) . Just

isPluginActive :: Plugin -> IO Bool
isPluginActive plugin = do
    state <- readIORef (plugin_state plugin)
    pure $ case state of
        Inactive -> False
        InactiveWithError -> False
        _ -> True

isPluginProcessing :: Plugin -> IO Bool
isPluginProcessing plugin = (== ActiveAndProcessing) <$> readIORef (plugin_state plugin)

isPluginSleeping :: Plugin -> IO Bool
isPluginSleeping plugin = (== ActiveAndSleeping) <$> readIORef (plugin_state plugin)