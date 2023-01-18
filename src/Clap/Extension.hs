{-# LANGUAGE MultiWayIf #-}

module Clap.Extension where

import Clap.Interface.Plugin as Plugin
import Clap.Interface.Extension.Foreign.Gui
import Clap.Interface.Extension.Foreign.Log
import Clap.Interface.Extension.Gui as Gui
import Clap.Interface.Extension.Log as Log
import Clap.Interface.Extension.Render as Render
import Debug.Trace
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Utils

data HostExtensions = HostExtensions
    { hostExtensions_gui :: HostGuiHandle
    , hostExtensions_log :: HostLogHandle
    } deriving (Show)

initializeExtensions :: IO HostExtensions
initializeExtensions = do
    gui <- new $ C'clap_host_gui 
        { c'clap_host_gui'resize_hints_changed = nullFunPtr
        , c'clap_host_gui'request_resize = nullFunPtr
        , c'clap_host_gui'request_show = nullFunPtr
        , c'clap_host_gui'request_hide = nullFunPtr
        , c'clap_host_gui'closed = nullFunPtr
        }
    log <- createHostLog $ \hostHandle logLevel message ->
        putStrLn $ Prelude.show logLevel <> ": " <> message 
    pure $ HostExtensions
        { hostExtensions_gui = gui
        , hostExtensions_log = log 
        }

getExtension :: HostExtensions -> String -> IO (Ptr ())
getExtension extensions name = pure $ if
    | name == Gui.extensionId -> castPtr $ hostExtensions_gui extensions
    | name == Log.extensionId -> castPtr $ hostExtensions_log extensions
    | otherwise -> nullPtr