module Clap.Interface.Extension.Render where

import Clap.Interface.Extension.Foreign.Render
import Clap.Interface.Plugin
import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable

extensionId :: String
extensionId = "clap.render"

newtype PluginRenderHandle = PluginRenderHandle { unPluginRenderHandle :: Ptr C'clap_plugin_render }
    deriving (Show)

data RenderMode 
    = RenderRealtime
    | RenderOffline 
    deriving (Enum)

hasHardRealtimeRequirement :: PluginRenderHandle -> PluginHandle -> IO Bool
hasHardRealtimeRequirement (PluginRenderHandle pluginRender) (PluginHandle plugin) = do
    funPtr <- peek $ p'clap_plugin_render'has_hard_realtime_requirement pluginRender
    pure $ toBool $ mK'has_hard_realtime_requirement funPtr plugin

set :: PluginRenderHandle -> PluginHandle -> RenderMode -> IO Bool
set (PluginRenderHandle pluginRender) (PluginHandle plugin) renderMode = do
    funPtr <- peek $ p'clap_plugin_render'set pluginRender
    pure $ toBool $ mK'set funPtr plugin (fromIntegral $ fromEnum renderMode)