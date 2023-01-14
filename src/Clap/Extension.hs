{-# LANGUAGE MultiWayIf #-}

module Clap.Extension where

import Clap.Interface.Plugin as Plugin
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
    <$> initializeExtension Gui.extensionId
    <*> initializeExtension Log.extensionId
    <*> initializeExtension Render.extensionId
    where 
        initializeExtension id =
            fmap castPtr <$> Plugin.getExtension plugin id 

getExtension :: Extensions -> String -> Ptr ()
getExtension extensions name = if
    | name == Gui.extensionId -> extract extensions_gui
    | name == Log.extensionId -> extract extensions_log
    | name == Render.extensionId -> extract extensions_render
    | otherwise -> nullPtr
    where
        extract selector =
            maybe nullPtr castPtr (selector extensions)