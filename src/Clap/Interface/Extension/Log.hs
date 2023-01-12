module Clap.Interface.Extension.Log where

import Clap.Interface.Extension.Foreign.Log
import Clap.Interface.Host
import Foreign.C.String
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

extensionId :: String
extensionId = "clap.log"

newtype HostLogHandle = HostLogHandle { unHostLogHandle :: Ptr C'clap_host_log }
    deriving (Show)

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
        let hostHandle = HostHandle cHostHandle
        let logLevel = toEnum $ fromIntegral cLogLevel
        message <- peekCString cMessage
        logCallback hostHandle logLevel message)
    hostLogC <- new $ C'clap_host_log
        { c'clap_host_log'log = logCallbackC }
    pure $ HostLogHandle hostLogC
