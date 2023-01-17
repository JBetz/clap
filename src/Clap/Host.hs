module Clap.Host where

import Clap.Extension
import Clap.Interface.AudioBuffer
import Clap.Interface.Entry as Entry
import Clap.Interface.Plugin as Plugin
import Clap.Interface.Events
import Clap.Interface.PluginFactory
import Clap.Interface.Host as Host
import Clap.Interface.Foreign
import Clap.Interface.Process
import Clap.Interface.Version
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
import System.Posix.DynamicLinker

data ThreadType 
    = MainThread
    | AudioThread
    | Unknown

data PluginHost = PluginHost
    { pluginHost_handle :: HostHandle
    , pluginHost_plugins :: IORef (Map PluginId Plugin)
    , pluginHost_threadType :: IORef ThreadType
    , pluginHost_process :: ProcessHandle
    , pluginHost_audioIn :: AudioBufferHandle
    , pluginHost_audioOut :: AudioBufferHandle
    , pluginHost_inputEvents :: InputEventsHandle
    , pluginHost_outputEvents :: OutputEventsHandle
    }

type PluginId = (FilePath, Int)

data Plugin = Plugin
    { plugin_library :: PluginLibrary
    , plugin_entry :: PluginEntryHandle
    , plugin_factory :: PluginFactoryHandle
    , plugin_descriptor :: PluginDescriptor
    , plugin_handle :: PluginHandle
    , plugin_extensions :: Extensions
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
    | InvalidPluginId
    deriving (Show)

instance Exception PluginException

createPluginHost :: HostConfig -> IO PluginHost
createPluginHost hostConfig = do
    hostHandle <- Host.createHost hostConfig
    plugins <- newIORef mempty
    threadType <- newIORef Unknown
    process <- newIORef Nothing
    audioIn <- newIORef nullPtr
    audioOut <- newIORef nullPtr
    pure $ PluginHost
        { pluginHost_handle = hostHandle 
        , pluginHost_plugins = plugins
        , pluginHost_threadType = threadType
        , pluginHost_process = nullPtr
        , pluginHost_audioIn = nullPtr
        , pluginHost_audioOut = nullPtr
        , pluginHost_inputEvents = undefined
        , pluginHost_outputEvents = undefined
        }

load :: PluginHost -> PluginId -> IO ()
load host (filePath, index) = do
    let hostHandle = pluginHost_handle host 
    withPluginLibrary filePath $ \library -> do
        entry <- clapEntry library
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
                        let hostHandle = pluginHost_handle host
                        count <- getPluginCount factory
                        maybePluginInstanceHandle <- createPlugin factory hostHandle (pluginDescriptor_id descriptor) 
                        case maybePluginInstanceHandle of
                            Nothing -> throw CreationFailed
                            Just pluginInstanceHandle -> do 
                                isPluginInitialized <- Plugin.init pluginInstanceHandle
                                unless isPluginInitialized $ throw PluginInitializationFailed
                                extensions <- initializeExtensions pluginInstanceHandle               
                                addPlugin host (filePath, index) $ Plugin
                                    { plugin_library = library
                                    , plugin_entry = entry
                                    , plugin_factory = factory
                                    , plugin_descriptor = descriptor
                                    , plugin_handle = pluginInstanceHandle
                                    , plugin_extensions = extensions
                                    }
    where        
        addPlugin :: PluginHost -> PluginId -> Plugin -> IO ()
        addPlugin host key plugin =
            modifyIORef (pluginHost_plugins host) $ Map.insert key plugin

activate :: PluginHost -> PluginId -> Double -> Int32 -> IO Bool
activate host pluginId sampleRate blockSize = do
    plugin <- getPlugin host pluginId
    Plugin.activate (plugin_handle plugin) sampleRate blockSize blockSize

activateAll :: PluginHost -> Double -> Int32 -> IO ()
activateAll host sampleRate blockSize = do
    plugins <- readIORef (pluginHost_plugins host) 
    for_ (Map.elems plugins) $ \plugin ->
        Plugin.activate (plugin_handle plugin) sampleRate blockSize blockSize


deactivate :: PluginHost -> PluginId -> IO ()
deactivate host pluginId = do
    plugin <- getPlugin host pluginId
    Plugin.deactivate (plugin_handle plugin)

processBegin :: PluginHost -> Word64 -> Int64 -> IO ()
processBegin host framesCount steadyTime = do
    setThreadType host AudioThread
    let process = pluginHost_process host
    setFramesCount process framesCount
    setSteadyTime process steadyTime
    
processEvent :: PluginHost -> EventConfig -> Event -> IO Bool
processEvent host eventConfig event =
    tryPush (pluginHost_outputEvents host) eventConfig event

process :: PluginHost -> IO ()
process host = do
    let process = pluginHost_process host
    setTransport process nullPtr
    setInputEvents process (pluginHost_inputEvents host)
    setOutputEvents process (pluginHost_outputEvents host)
    setAudioInputs process (pluginHost_audioIn host)
    setAudioInputsCount process 1
    setAudioOutputs process (pluginHost_audioOut host)
    setAudioOutputsCount process 1
    plugins <- readIORef $ pluginHost_plugins host
    for_ (Map.elems plugins) $ \plugin -> do
        startProcessing (plugin_handle plugin)
        Plugin.process (plugin_handle plugin) process


processEnd :: PluginHost -> Word64 -> Int64 -> IO ()
processEnd host numberOfFrames steadyTime = do
    setThreadType host Unknown
    let process = pluginHost_process host
    setFramesCount process numberOfFrames
    setSteadyTime process steadyTime

setThreadType :: PluginHost -> ThreadType -> IO ()
setThreadType host =
    writeIORef (pluginHost_threadType host)

setPorts :: PluginHost -> BufferData t -> BufferData t -> IO ()
setPorts host inputs outputs = do
    let audioIn = pluginHost_audioIn host
    setChannelCount audioIn 0
    setBufferData audioIn inputs
    setConstantMask audioIn 0
    setLatency audioIn 0

    let audioOut = pluginHost_audioOut host
    setChannelCount audioOut 0
    setBufferData audioOut outputs
    setConstantMask audioOut 0
    setLatency audioOut 0

getPlugin :: PluginHost -> PluginId -> IO Plugin
getPlugin host pluginId = do
    plugins <- readIORef $ pluginHost_plugins host
    case Map.lookup pluginId plugins of
        Nothing -> throw InvalidPluginId
        Just plugin -> pure plugin

withPluginLibrary :: FilePath -> (PluginLibrary -> IO a) -> IO a
withPluginLibrary filePath f = 
    withDL filePath [RTLD_NOW] (f . PluginLibrary)