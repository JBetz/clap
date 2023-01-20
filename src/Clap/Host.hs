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
    , pluginHost_process :: ProcessHandle
    , pluginHost_audioIn :: AudioBufferHandle
    , pluginHost_audioOut :: AudioBufferHandle
    , pluginHost_events :: IORef [EventHandle]
    , pluginHost_inputEvents :: InputEventsHandle
    , pluginHost_outputEvents :: OutputEventsHandle
    , pluginHost_extensions :: HostExtensions
    }

type PluginId = (FilePath, Int)

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
    , plugin_processStatus :: IORef (Maybe ProcessStatus)
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
    process' <- createProcess
    audioIn <- createAudioBuffer
    audioOut <- createAudioBuffer
    events <- newIORef []
    inputEvents <- createInputEvents events
    outputEvents <- createOutputEvents
    pure $ PluginHost
        { pluginHost_handle = hostHandle 
        , pluginHost_plugins = plugins
        , pluginHost_threadType = threadType
        , pluginHost_process = process'
        , pluginHost_audioIn = audioIn
        , pluginHost_audioOut = audioOut
        , pluginHost_events = events
        , pluginHost_inputEvents = inputEvents
        , pluginHost_outputEvents = outputEvents
        , pluginHost_extensions = extensions
        }

load :: PluginHost -> PluginId -> IO ()
load host (filePath, index) = do
    let hostHandle = pluginHost_handle host 
    library <- openPluginLibrary filePath
    entry <- lookupPluginEntry library
    isEntryInitialized <- Entry.init entry filePath
    unless isEntryInitialized $ throw EntryInitializationFailed
    maybeFactory <- getFactory entry pluginFactoryId
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
                        addPlugin (filePath, index) $ Plugin
                            { plugin_library = library
                            , plugin_entry = entry
                            , plugin_factory = factory
                            , plugin_descriptor = descriptor
                            , plugin_handle = pluginHandle
                            , plugin_state = state
                            , plugin_processStatus = processStatus
                            }
    where        
        addPlugin :: PluginId -> Plugin -> IO ()
        addPlugin key plugin =
            modifyIORef' (pluginHost_plugins host) $ Map.insert key plugin

activate :: PluginHost -> PluginId -> Double -> Word32 -> IO ()
activate host pluginId sampleRate blockSize = do
    plugin <- getPlugin host pluginId
    isActivated <- Plugin.activate (plugin_handle plugin) sampleRate blockSize blockSize
    setState plugin $ if isActivated 
        then ActiveAndSleeping
        else InactiveWithError 

activateAll :: PluginHost -> Double -> Word32 -> IO ()
activateAll host sampleRate blockSize = do
    plugins <- readIORef (pluginHost_plugins host) 
    for_ (Map.keys plugins) $ \pluginId ->
        activate host pluginId sampleRate blockSize

deactivate :: PluginHost -> PluginId -> IO ()
deactivate host pluginId = do
    plugin <- getPlugin host pluginId
    whenM (isPluginActive plugin) $ do 
        Plugin.deactivate (plugin_handle plugin)
        setState plugin Inactive

deactivateAll :: PluginHost -> IO ()
deactivateAll host = do
    plugins <- readIORef (pluginHost_plugins host) 
    for_ (Map.keys plugins) $ deactivate host

processBegin :: PluginHost -> Word64 -> Int64 -> IO ()
processBegin host framesCount steadyTime = do
    setThreadType host AudioThread
    let process' = pluginHost_process host
    setFramesCount process' framesCount
    setSteadyTime process' steadyTime
    
processEvent :: PluginHost -> EventConfig -> Event -> IO ()
processEvent host eventConfig event =
    push (pluginHost_events host) eventConfig event

process :: PluginHost -> IO ()
process host = do
    let process' = pluginHost_process host
    setTransport process' nullPtr
    setInputEvents process' (pluginHost_inputEvents host)
    setOutputEvents process' (pluginHost_outputEvents host)
    setAudioInputs process' (pluginHost_audioIn host)
    setAudioInputsCount process' 1
    setAudioOutputs process' (pluginHost_audioOut host)
    setAudioOutputsCount process' 1
    plugins <- readIORef $ pluginHost_plugins host
    for_ (Map.elems plugins) $ \plugin -> do 
        whenM (isPluginSleeping plugin) $ do
            isStarted <- Plugin.startProcessing (plugin_handle plugin)
            setState plugin $ if isStarted 
                then ActiveAndProcessing
                else ActiveWithError
        whenM (isPluginProcessing plugin) $ do
            !status <- Plugin.process (plugin_handle plugin) process'
            setProcessStatus plugin status

processEnd :: PluginHost -> Word64 -> Int64 -> IO ()
processEnd host numberOfFrames steadyTime = do
    setThreadType host Unknown
    let process' = pluginHost_process host
    setFramesCount process' numberOfFrames
    setSteadyTime process' steadyTime

setThreadType :: PluginHost -> ThreadType -> IO ()
setThreadType host =
    writeIORef (pluginHost_threadType host)

setPorts :: PluginHost -> BufferData t -> BufferData t -> IO ()
setPorts host inputs outputs = do
    let audioIn = pluginHost_audioIn host
    setChannelCount audioIn 2
    setBufferData audioIn inputs
    setConstantMask audioIn 0
    setLatency audioIn 0
    
    let audioOut = pluginHost_audioOut host
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