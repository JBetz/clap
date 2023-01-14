module Clap.Interface.Extension.Log where

import Clap.Interface.Extension.Foreign.Log
import Clap.Interface.Host
import Foreign.C.String
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

extensionId :: String
extensionId = "clap.log"

type HostLogHandle = Ptr C'clap_host_log

data LogLevel 
    = LogDebug
    | LogInfo
    | LogWarning
    | LogError
    | LogFatal
    | LogHostMisbehaving
    | LogPluginMisbehaving
    deriving (Enum)

createHostLog :: (HostHandle -> LogLevel -> String -> IO ()) -> IO HostLogHandle
createHostLog logCallback = do
    logCallbackC <- mk'log (\cHostHandle cLogLevel cMessage -> do
        let logLevel = toEnum $ fromIntegral cLogLevel
        message <- peekCString cMessage
        logCallback cHostHandle logLevel message)
    new $ C'clap_host_log
        { c'clap_host_log'log = logCallbackC }
    