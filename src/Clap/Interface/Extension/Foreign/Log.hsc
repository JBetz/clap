{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <ext/log.h>
module Clap.Interface.Extension.Foreign.Log where
import Foreign.Ptr
#strict_import

import Clap.Interface.Foreign.Plugin
import Clap.Interface.Foreign.Host

-- #globalarray CLAP_EXT_LOG , CChar
{- enum {
    CLAP_LOG_DEBUG = 0,
    CLAP_LOG_INFO = 1,
    CLAP_LOG_WARNING = 2,
    CLAP_LOG_ERROR = 3,
    CLAP_LOG_FATAL = 4,
    CLAP_LOG_HOST_MISBEHAVING = 5,
    CLAP_LOG_PLUGIN_MISBEHAVING = 6
}; -}
#num CLAP_LOG_DEBUG
#num CLAP_LOG_INFO
#num CLAP_LOG_WARNING
#num CLAP_LOG_ERROR
#num CLAP_LOG_FATAL
#num CLAP_LOG_HOST_MISBEHAVING
#num CLAP_LOG_PLUGIN_MISBEHAVING
{- typedef int32_t clap_log_severity; -}
#synonym_t clap_log_severity , CInt
{- typedef struct clap_host_log {
            void (* log)(const clap_host_t * host,
                         clap_log_severity severity,
                         const char * msg);
        } clap_host_log_t; -}
#starttype struct clap_host_log
#field log , FunPtr (Ptr <struct clap_host> -> CInt -> CString -> IO ())
#stoptype
#synonym_t clap_host_log_t , <struct clap_host_log>
#callback_t log , Ptr <struct clap_host> -> CInt -> CString -> IO ()