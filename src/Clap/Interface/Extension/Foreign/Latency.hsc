{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <clap/ext/latency.h>
module Clap.Interface.Extension.Foreign.Latency where
import Foreign.Ptr
#strict_import

import Clap.Interface.Foreign.Host
import Clap.Interface.Foreign.Plugin
#globalarray CLAP_EXT_LATENCY , CChar
{- typedef struct clap_plugin_latency {
            uint32_t (* get)(const clap_plugin_t * plugin);
        } clap_plugin_latency_t; -}
#starttype struct clap_plugin_latency
#field get , FunPtr (Ptr <struct clap_plugin> -> CUInt)
#stoptype
#synonym_t clap_plugin_latency_t , <struct clap_plugin_latency>
#callback_t get , Ptr <struct clap_plugin> -> CUInt
{- typedef struct clap_host_latency {
            void (* changed)(const clap_host_t * host);
        } clap_host_latency_t; -}
#starttype struct clap_host_latency
#field changed , FunPtr (Ptr <struct clap_host> -> IO ())
#stoptype
#synonym_t clap_host_latency_t , <struct clap_host_latency>
#callback_t changed , Ptr <struct clap_host> -> IO ()