{-# LANGUAGE MultiWayIf #-}

module Clap.Extension where

import Clap.Interface.Plugin
import Clap.Interface.Extension.Foreign.Gui
import Clap.Interface.Extension.Params as Params
import Clap.Interface.Extension.Gui as Gui
import Clap.Interface.Extension.Log as Log
import Foreign.Ptr
import Foreign.Marshal.Utils

data HostExtensions = HostExtensions
    { hostExtensions_gui :: HostGuiHandle
    , hostExtensions_log :: HostLogHandle
    } deriving (Show)

initializeHostExtensions :: IO HostExtensions
initializeHostExtensions = do
    resizeHintsChangedC <- mk'resize_hints_changed $ \_host -> print "resize_hints_changed"
    requestResizeC <- mk'request_resize $ \_host _width _height -> print "request resize" >> pure (fromBool False)
    requestShowC <- mk'request_show $ \_host -> print "request_show" >> pure (fromBool False)
    requestHideC <- mk'request_hide $ \_host -> print "request_hide" >> pure (fromBool False)
    closedC <- mk'closed $ \_host _wasDestroyed -> print "closed" >> pure ()
    guiHandle <- new $ C'clap_host_gui 
        { c'clap_host_gui'resize_hints_changed = resizeHintsChangedC
        , c'clap_host_gui'request_resize = requestResizeC
        , c'clap_host_gui'request_show = requestShowC
        , c'clap_host_gui'request_hide = requestHideC
        , c'clap_host_gui'closed = closedC
        }
    logHandle <- createHostLog $ \_hostHandle logLevel message ->
        putStrLn $ Prelude.show logLevel <> ": " <> message 
    pure $ HostExtensions
        { hostExtensions_gui = guiHandle
        , hostExtensions_log = logHandle
        }

getHostExtension :: HostExtensions -> String -> IO (Ptr ())
getHostExtension extensions name = pure $ if
    | name == Gui.extensionId -> castPtr $ hostExtensions_gui extensions
    | name == Log.extensionId -> castPtr $ hostExtensions_log extensions
    | otherwise -> nullPtr

data PluginExtensions = PluginExtensions
    { pluginExtensions_gui :: Maybe PluginGuiHandle
    , pluginExtensions_params :: Maybe PluginParametersHandle 
    }

initializePluginExtensions :: PluginHandle -> IO PluginExtensions
initializePluginExtensions pluginHandle = do
    maybeGuiHandle <- getPluginExtension pluginHandle Gui.extensionId
    maybeParamsHandle <- getPluginExtension pluginHandle Params.extensionId
    pure $ PluginExtensions 
        { pluginExtensions_gui = maybeGuiHandle
        , pluginExtensions_params = maybeParamsHandle 
        }
