module Clap.Extension where

import Clap.Interface.Plugin
import Clap.Interface.Extension.Foreign.Gui
import Clap.Interface.Extension.Gui as Gui
import Clap.Interface.Extension.Log as Log
import Foreign.Ptr

data Extensions = Extensions
    { extensions_gui :: Maybe PluginGuiHandle
    , extensions_log :: Maybe ()
    } deriving (Show)

initializeExtensions :: PluginHandle ->  IO Extensions
initializeExtensions plugin = Extensions
    <$> (fmap (PluginGuiHandle . castPtr) <$> getExtension plugin Gui.extensionId)
    <*> fmap (const Nothing) (getExtension plugin Log.extensionId)