module Clap.Interface.Plugin where

import Clap.Interface.Foreign.Plugin
import Clap.Interface.PluginFeatures
import Clap.Interface.Process
import Clap.Interface.Version
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

type PluginHandle = Ptr C'clap_plugin

data PluginDescriptor = PluginDescriptor 
    { pluginDescriptor_clapVersion :: ClapVersion
    , pluginDescriptor_id :: String
    , pluginDescriptor_name :: String
    , pluginDescriptor_vendor :: String
    , pluginDescriptor_url :: String
    , pluginDescriptor_manualUrl :: String
    , pluginDescriptor_supportUrl :: String
    , pluginDescriptor_version :: String
    , pluginDescriptor_description :: String
    , pluginDescriptor_features :: [PluginFeature]
    } deriving (Eq, Show)

fromStruct :: C'clap_plugin_descriptor -> IO PluginDescriptor
fromStruct struct = do 
    let clapVersion = Clap.Interface.Version.fromStruct (c'clap_plugin_descriptor'clap_version struct)
    id' <- peekCString (c'clap_plugin_descriptor'id struct)
    name <- peekCString (c'clap_plugin_descriptor'name struct)
    vendor <- peekCString (c'clap_plugin_descriptor'vendor struct)
    url <- peekCString (c'clap_plugin_descriptor'url struct)
    manualUrl <- peekCString (c'clap_plugin_descriptor'manual_url struct)
    supportUrl <- peekCString (c'clap_plugin_descriptor'support_url struct)
    version <- peekCString (c'clap_plugin_descriptor'version struct)
    description <- peekCString (c'clap_plugin_descriptor'description struct)
    featuresC <- peekArray0 nullPtr (c'clap_plugin_descriptor'features struct)
    features <- traverse (fmap read . peekCString) featuresC
    pure $ PluginDescriptor 
        { pluginDescriptor_clapVersion = clapVersion
        , pluginDescriptor_id = id'
        , pluginDescriptor_name = name
        , pluginDescriptor_vendor = vendor
        , pluginDescriptor_url = url
        , pluginDescriptor_manualUrl = manualUrl
        , pluginDescriptor_supportUrl = supportUrl
        , pluginDescriptor_version = version
        , pluginDescriptor_description = description
        , pluginDescriptor_features = features
        }

init :: PluginHandle -> IO Bool
init plugin = do
    funPtr <- peek $ p'clap_plugin'init plugin
    pure $ toBool (mK'init funPtr plugin)

destroy :: PluginHandle -> IO ()
destroy plugin = do
    funPtr <- peek $ p'clap_plugin'destroy plugin
    mK'destroy funPtr plugin

activate :: PluginHandle -> Double -> Word32 -> Word32 -> IO Bool
activate plugin sampleRate minFramesCount maxFramesCount = do
    funPtr <- peek $ p'clap_plugin'activate plugin
    pure $ toBool $ mK'activate funPtr plugin  (CDouble sampleRate) (fromIntegral minFramesCount) (fromIntegral maxFramesCount)

deactivate :: PluginHandle -> IO ()
deactivate plugin = do
    funPtr <- peek $ p'clap_plugin'deactivate plugin
    mK'deactivate funPtr plugin

startProcessing :: PluginHandle -> IO Bool
startProcessing plugin = do
    funPtr <- peek $ p'clap_plugin'start_processing plugin
    pure $ toBool (mK'start_processing funPtr plugin)

stopProcessing :: PluginHandle -> IO ()
stopProcessing plugin = do
    funPtr <- peek $ p'clap_plugin'stop_processing plugin
    mK'stop_processing funPtr plugin

reset :: PluginHandle -> IO ()
reset plugin = do
    funPtr <- peek $ p'clap_plugin'reset plugin
    mK'reset funPtr plugin

process :: PluginHandle -> ProcessHandle -> IO ProcessStatus
process plugin process = do
    funPtr <- peek $ p'clap_plugin'process plugin
    pure $ toEnum $ fromIntegral $ mK'process funPtr plugin process

getExtension :: PluginHandle -> String -> IO (Maybe (Ptr ()))
getExtension plugin name = do
    funPtr <- peek $ p'clap_plugin'get_extension plugin
    withCString name $ \cName -> do
        let extension = mK'get_extension funPtr plugin cName
        pure $ if extension == nullPtr
            then Nothing
            else Just extension

onMainThread :: PluginHandle -> IO ()
onMainThread plugin = do
    funPtr <- peek $ p'clap_plugin'on_main_thread plugin
    mK'on_main_thread funPtr plugin
