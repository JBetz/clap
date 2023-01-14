module Clap.Interface.Extension.Tail where

import Clap.Interface.Extension.Foreign.Tail
import Clap.Interface.Plugin
import Clap.Interface.Host
import Data.Word
import Foreign.Ptr
import Foreign.Storable

type PluginTailHandle = Ptr C'clap_plugin_tail

get :: PluginTailHandle -> PluginHandle -> IO Word32
get pluginTail plugin = do
    funPtr <- peek $ p'clap_plugin_tail'get pluginTail
    pure $ fromIntegral $ mK'get funPtr plugin

type HostTailHandle = Ptr C'clap_host_tail

changed :: HostTailHandle -> HostHandle -> IO ()
changed hostTail host = do
    funPtr <- peek $ p'clap_host_tail'changed hostTail
    mK'changed funPtr host
