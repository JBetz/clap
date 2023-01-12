{-# LANGUAGE RankNTypes #-}

module Clap.Interface.Host where

import Clap.Interface.Foreign.Host
import Clap.Interface.Version
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

newtype HostHandle = HostHandle { unHostHandle :: Ptr C'clap_host }

data HostConfig d = HostConfig
    { hostConfig_clapVersion :: ClapVersion
    , hostConfig_data :: Storable d => d
    , hostConfig_name :: String
    , hostConfig_vendor :: String
    , hostConfig_url :: String
    , hostConfig_version :: String
    , hostConfig_getExtension :: HostHandle -> String -> IO (Ptr ()) 
    , hostConfig_requestRestart :: HostHandle -> IO ()
    , hostConfig_requestProcess :: HostHandle -> IO ()
    , hostConfig_requestCallback :: HostHandle -> IO ()
    }

createHost :: Storable d => HostConfig d -> IO HostHandle
createHost (HostConfig clapVersion data' name vendor url version getExtension requestRestart requestProcess requestCallback) = do
    let clapVersionC = toStruct clapVersion
    dataC <- new data'
    nameC <- newCString name
    vendorC <- newCString vendor
    urlC <- newCString url
    versionC <- newCString version
    getExtensionC <- Clap.Interface.Foreign.Host.mk'get_extension (\cHostHandle cString -> do
        let hostHandle = HostHandle cHostHandle
        string <- peekCString cString
        getExtension hostHandle string)
    requestRestartC <- mk'request_restart (requestRestart . HostHandle)
    requestProcessC <- mk'request_process (requestProcess . HostHandle)
    requestCallbackC <- mk'request_callback (requestCallback . HostHandle)
    hostC <- new $ C'clap_host
        { c'clap_host'clap_version = clapVersionC
        , c'clap_host'host_data = castPtr dataC
        , c'clap_host'name = nameC
        , c'clap_host'vendor = vendorC
        , c'clap_host'url = urlC
        , c'clap_host'version = versionC
        , c'clap_host'get_extension = getExtensionC
        , c'clap_host'request_restart = requestRestartC
        , c'clap_host'request_process = requestProcessC
        , c'clap_host'request_callback = requestCallbackC
        }
    pure $ HostHandle hostC
