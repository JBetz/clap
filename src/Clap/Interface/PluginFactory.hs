module Clap.Interface.PluginFactory where

import Clap.Interface.Host
import Clap.Interface.Plugin
import Clap.Interface.Foreign
import Clap.Interface.Foreign.PluginFactory
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable

pluginFactoryId :: String
pluginFactoryId = "clap.plugin-factory"

type PluginFactoryHandle = Ptr C'clap_plugin_factory

getPluginCount :: PluginFactoryHandle -> IO Int
getPluginCount pluginFactory = do
    getPluginCountPtr <- peek $ p'clap_plugin_factory'get_plugin_count pluginFactory
    pure $ fromIntegral $ mK'get_plugin_count getPluginCountPtr pluginFactory

getPluginDescriptor :: PluginFactoryHandle -> Int -> IO (Maybe PluginDescriptor)
getPluginDescriptor pluginFactory index = do
    getPluginDescriptorPtr <- peek $ p'clap_plugin_factory'get_plugin_descriptor pluginFactory
    let descriptorPtr = mK'get_plugin_descriptor getPluginDescriptorPtr pluginFactory (fromIntegral index) 
    if descriptorPtr /= nullPtr
        then do 
            cDescriptor <- peek $ castPtr descriptorPtr
            descriptor <- Clap.Interface.Plugin.fromStruct cDescriptor
            pure $ Just descriptor
        else pure Nothing

createPlugin :: PluginFactoryHandle -> HostHandle -> String -> IO (Maybe PluginHandle)
createPlugin pluginFactory host pluginId = do
    createPluginPtr <- peek $ p'clap_plugin_factory'create_plugin pluginFactory
    withCString pluginId $ \cPluginId -> do 
        let plugin = mK'create_plugin createPluginPtr pluginFactory host cPluginId
        pure $ pureIf (plugin /= nullPtr) plugin
