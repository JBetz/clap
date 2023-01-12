{-# LANGUAGE LambdaCase #-}

module Clap.Interface.Extension.Gui where

import Clap.Interface.Plugin
import Clap.Interface.Extension.Foreign.Gui
import Data.Char
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Utils

extensionId :: String
extensionId = "clap.gui"

data WindowAPI 
    = Win32
    | Cocoa
    | X11
    | Wayland

instance Show WindowAPI where
    show = \case
        Win32 -> "win32"
        Cocoa -> "cocoa"
        X11 -> "x11"
        Wayland -> "wayland"

newtype PluginGuiHandle = PluginGuiHandle { unPluginGuiHandle :: Ptr C'clap_plugin_gui }
    deriving (Show)

newtype GuiResizeHintsHandle = GuiResizeHintsHandle { unGuiResizeHintsHandle :: Ptr C'clap_gui_resize_hints }
    deriving (Show)

newtype WindowHandle = WindowHandle { unWindowHandle :: Ptr C'clap_window }
    deriving (Show)

createWindow :: WindowAPI -> Ptr () -> IO WindowHandle
createWindow api handle = do
    cApi <- newCString $ Prelude.show api
    cWindow <- new $ C'clap_window
        { c'clap_window'api = cApi
        , c'clap_window'handle = handle
        }
    pure $ WindowHandle cWindow

isApiSupported :: PluginGuiHandle -> PluginHandle -> WindowAPI -> Bool -> IO Bool
isApiSupported (PluginGuiHandle pluginGui) (PluginHandle plugin) windowApi isFloating = do
    funPtr <- peek (p'clap_plugin_gui'is_api_supported pluginGui)
    withCString (Prelude.show windowApi) $ \cWindowApi ->
        pure $ toBool $ mK'is_api_supported funPtr plugin cWindowApi (fromBool isFloating)

getPreferredApi :: PluginGuiHandle -> PluginHandle -> WindowAPI -> Bool -> IO Bool
getPreferredApi (PluginGuiHandle pluginGui) (PluginHandle plugin) windowApi isFloating = do
    funPtr <- peek $ p'clap_plugin_gui'get_preferred_api pluginGui
    -- TODO: Why does they require creating a new pointer?
    cWindowApi <- new $ case windowApi of 
        Win32 -> c'CLAP_WINDOW_API_WIN32
        Cocoa -> c'CLAP_WINDOW_API_COCOA
        X11 -> c'CLAP_WINDOW_API_X11
        Wayland -> c'CLAP_WINDOW_API_WAYLAND
    ptrIsFloating <- new $ fromBool isFloating
    pure $ toBool $ mK'get_preferred_api funPtr plugin cWindowApi ptrIsFloating

create :: PluginGuiHandle -> PluginHandle -> WindowAPI -> Bool -> IO Bool
create (PluginGuiHandle pluginGui) (PluginHandle plugin) windowApi isFloating = do
    funPtr <- peek $ p'clap_plugin_gui'create pluginGui
    withCString (Prelude.show windowApi) $ \cWindowApi ->
        pure $ toBool $ mK'create funPtr plugin cWindowApi (fromBool isFloating)

destroy :: PluginGuiHandle -> PluginHandle -> IO ()
destroy (PluginGuiHandle pluginGui) (PluginHandle plugin) = do
    funPtr <- peek $ p'clap_plugin_gui'destroy pluginGui
    mK'destroy funPtr plugin

setScale :: PluginGuiHandle -> PluginHandle -> Double -> IO Bool
setScale (PluginGuiHandle pluginGui) (PluginHandle plugin) scale = do
    funPtr <- peek $ p'clap_plugin_gui'set_scale pluginGui
    pure $ toBool $ mK'set_scale funPtr plugin (CDouble scale)

getSize :: PluginGuiHandle -> PluginHandle -> Int -> Int -> IO Int
getSize (PluginGuiHandle pluginGui) (PluginHandle plugin) width height = do
    funPtr <- peek $ p'clap_plugin_gui'get_size pluginGui
    cWidth <- new $ fromIntegral width
    cHeight <- new $ fromIntegral height
    pure $ fromIntegral $ mK'get_size funPtr plugin cWidth cHeight

canResize :: PluginGuiHandle -> PluginHandle -> IO Bool
canResize (PluginGuiHandle pluginGui) (PluginHandle plugin) = do
    funPtr <- peek $ p'clap_plugin_gui'can_resize pluginGui
    pure $ toBool $ mK'can_resize funPtr plugin

getResizeHints :: PluginGuiHandle -> PluginHandle -> GuiResizeHintsHandle -> IO Bool
getResizeHints (PluginGuiHandle pluginGui) (PluginHandle plugin) (GuiResizeHintsHandle guiResizeHints) = do
    funPtr <- peek $ p'clap_plugin_gui'get_resize_hints pluginGui
    pure $ toBool $ mK'get_resize_hints funPtr plugin guiResizeHints

adjustSize :: PluginGuiHandle -> PluginHandle -> Int -> Int -> IO Int
adjustSize (PluginGuiHandle pluginGui) (PluginHandle plugin) width height = do
    funPtr <- peek $ p'clap_plugin_gui'adjust_size pluginGui
    cWidth <- new $ fromIntegral width
    cHeight <- new $ fromIntegral height
    pure $ fromIntegral $ mK'adjust_size funPtr plugin cWidth cHeight

setSize :: PluginGuiHandle -> PluginHandle -> Int -> Int -> IO Int
setSize (PluginGuiHandle pluginGui) (PluginHandle plugin) width height = do
    funPtr <- peek $ p'clap_plugin_gui'set_size pluginGui
    pure $ fromIntegral $ mK'set_size funPtr plugin (fromIntegral width) (fromIntegral height)

setParent :: PluginGuiHandle -> PluginHandle -> WindowHandle -> IO Bool
setParent (PluginGuiHandle pluginGui) (PluginHandle plugin) (WindowHandle window) = do
    funPtr <- peek $ p'clap_plugin_gui'set_parent pluginGui
    pure $ toBool $ mK'set_parent funPtr plugin window

setTransient :: PluginGuiHandle -> PluginHandle -> WindowHandle -> IO Bool
setTransient (PluginGuiHandle pluginGui) (PluginHandle plugin) (WindowHandle window) = do
    funPtr <- peek $ p'clap_plugin_gui'set_transient pluginGui
    pure $ toBool $ mK'set_transient funPtr plugin window

suggestTitle :: PluginGuiHandle -> PluginHandle -> String -> IO ()
suggestTitle (PluginGuiHandle pluginGui) (PluginHandle plugin) title = do
    funPtr <- peek $ p'clap_plugin_gui'suggest_title pluginGui
    withCString title $ \cTitle -> 
        mK'suggest_title funPtr plugin cTitle

show :: PluginGuiHandle -> PluginHandle -> IO Bool
show (PluginGuiHandle pluginGui) (PluginHandle plugin) = do
    funPtr <- peek $ p'clap_plugin_gui'show pluginGui
    pure $ toBool $ mK'show funPtr plugin

hide :: PluginGuiHandle -> PluginHandle -> IO Bool
hide (PluginGuiHandle pluginGui) (PluginHandle plugin) = do
    funPtr <- peek $ p'clap_plugin_gui'hide pluginGui
    pure $ toBool $ mK'hide funPtr plugin