module Clap.Extension where

import Clap.Interface.Plugin
import Clap.Interface.Extension.Foreign.Gui
import Clap.Interface.Extension.Gui as Gui
import Clap.Interface.Extension.Log as Log
import Clap.Interface.Extension.Render as Render
import Foreign.Ptr

data Extensions = Extensions
    { extensions_gui :: Maybe PluginGuiHandle
    , extensions_log :: Maybe HostLogHandle
    , extensions_render :: Maybe PluginRenderHandle
    } deriving (Show)

initializeExtensions :: PluginHandle ->  IO Extensions
initializeExtensions plugin = Extensions
    <$> initializeExtension PluginGuiHandle Gui.extensionId
    <*> initializeExtension HostLogHandle Log.extensionId
    <*> initializeExtension PluginRenderHandle Render.extensionId
    where 
        initializeExtension wrapper id =
            fmap (wrapper . castPtr) <$> getExtension plugin id 
