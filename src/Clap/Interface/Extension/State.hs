module Clap.Interface.Extension.State where

import Clap.Interface.Extension.Foreign.State
import Clap.Interface.Plugin
import Clap.Interface.Host
import Clap.Interface.Stream
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

type PluginStateHandle = Ptr C'clap_plugin_state

save :: PluginStateHandle -> PluginHandle -> OutputStreamHandle -> IO Bool
save pluginState plugin outputStream = do
    funPtr <- peek $ p'clap_plugin_state'save pluginState
    pure $ toBool $ mK'save funPtr plugin outputStream

load :: PluginStateHandle -> PluginHandle -> InputStreamHandle -> IO Bool
load pluginState plugin inputStream = do
    funPtr <- peek $ p'clap_plugin_state'load pluginState
    pure $ toBool $ mK'load funPtr plugin inputStream

type HostStateHandle = Ptr C'clap_host_state

markDirty :: HostStateHandle -> HostHandle -> IO ()
markDirty hostState host = do
    funPtr <- peek $ p'clap_host_state'mark_dirty hostState
    mK'mark_dirty funPtr host
