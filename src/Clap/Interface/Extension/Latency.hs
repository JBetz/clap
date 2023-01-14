module Clap.Interface.Extension.Latency where

import Clap.Interface.Extension.Foreign.Latency
import Clap.Interface.Plugin
import Clap.Interface.Host
import Data.Word
import Foreign.Ptr
import Foreign.Storable

type PluginLatencyHandle = Ptr C'clap_plugin_latency

get :: PluginLatencyHandle -> PluginHandle -> IO Word32
get pluginLatency plugin = do
    funPtr <- peek $ p'clap_plugin_latency'get pluginLatency
    pure $ fromIntegral $ mK'get funPtr plugin

type HostLatencyHandle = Ptr C'clap_host_latency


changed :: HostLatencyHandle -> HostHandle -> IO ()
changed hostLatency host = do
    funPtr <- peek $ p'clap_host_latency'changed hostLatency
    mK'changed funPtr host
