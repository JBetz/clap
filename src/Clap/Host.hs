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
    , pluginHost_plugins :: IORef (Map ClapId Plugin)
    , pluginHost_threadType :: IORef ThreadType
    , pluginHost_extensions :: HostExtensions
    }

newtype ClapId = ClapId (FilePath, Int)
    deriving (Eq, Ord, Show)

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

load :: PluginHost -> ClapId -> IO ()
load host (ClapId (filePath, index)) = do
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
                        events <- newIORef []
                        inputEvents <- createInputEvents events
                        outputEvents <- createOutputEvents
                        process' <- createProcess
                        audioIn <- createAudioBuffer
                        audioOut <- createAudioBuffer
                        addPlugin (ClapId (filePath, index)) $ Plugin
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
        addPlugin :: ClapId -> Plugin -> IO ()
        addPlugin key plugin =
            modifyIORef' (pluginHost_plugins host) $ Map.insert key plugin

activate :: PluginHost -> ClapId -> Double -> Word32 -> IO ()
activate host clapId sampleRate blockSize = do
    plugin <- getPlugin host clapId
    isActivated <- Plugin.activate (plugin_handle plugin) sampleRate blockSize blockSize
    setState plugin $ if isActivated 
        then ActiveAndSleeping
        else InactiveWithError 

activateAll :: PluginHost -> Double -> Word32 -> IO ()
activateAll host sampleRate blockSize = do
    plugins <- readIORef (pluginHost_plugins host) 
    for_ (Map.keys plugins) $ \clapId ->
        activate host clapId sampleRate blockSize

deactivate :: PluginHost -> ClapId -> IO ()
deactivate host clapId = do
    plugin <- getPlugin host clapId 
    whenM (isPluginActive plugin) $ do 
        Plugin.deactivate (plugin_handle plugin)
        setState plugin Inactive

deactivateAll :: PluginHost -> IO ()
deactivateAll host = do
    plugins <- readIORef (pluginHost_plugins host) 
    for_ (Map.keys plugins) $ deactivate host
    
processAll :: PluginHost -> Word64 -> Int64 -> IO ()
processAll host framesCount steadyTime = do
    setThreadType host AudioThread
    plugins <- readIORef (pluginHost_plugins host)
    for_ (Map.elems plugins) $ \plugin -> do
        processBegin plugin framesCount steadyTime
        process plugin
    setThreadType host Unknown

processBegin :: Plugin -> Word64 -> Int64 -> IO ()
processBegin plugin framesCount steadyTime = do
    let process' = plugin_process plugin
    setFramesCount process' framesCount
    setSteadyTime process' steadyTime

processEvent :: PluginHost -> ClapId -> EventConfig -> Event -> IO ()
processEvent host clapId eventConfig event = do
    plugin <- getPlugin host clapId
    push (plugin_events plugin) eventConfig event

process :: Plugin -> IO ()
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
        isStarted <- Plugin.startProcessing (plugin_handle plugin)
        setState plugin $ if isStarted 
            then ActiveAndProcessing
            else ActiveWithError
    whenM (isPluginProcessing plugin) $ do
        !status <- Plugin.process (plugin_handle plugin) process'
        setProcessStatus plugin status

processEnd :: Plugin -> Word64 -> Int64 -> IO ()
processEnd plugin numberOfFrames steadyTime = do
    let process' = plugin_process plugin
    setFramesCount process' numberOfFrames
    setSteadyTime process' steadyTime

setThreadType :: PluginHost -> ThreadType -> IO ()
setThreadType host =
    writeIORef (pluginHost_threadType host)

setPorts :: PluginHost -> BufferData t -> BufferData t -> IO ()
setPorts host inputs outputs = do
    plugins <- readIORef (pluginHost_plugins host)
    for_ (Map.elems plugins) setPluginPorts
    where 
        setPluginPorts plugin = do
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

getPlugin :: PluginHost -> ClapId -> IO Plugin
getPlugin host clapId = do
    plugins <- readIORef $ pluginHost_plugins host
    case Map.lookup clapId plugins of
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