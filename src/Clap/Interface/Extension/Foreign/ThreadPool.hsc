{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include "include/ext/thread-pool.h"
module Clap.Interface.Extension.Foreign.ThreadPool where
import Foreign.Ptr
#strict_import

import Clap.Interface.Extension.Foreign....Plugin
#globalarray CLAP_EXT_THREAD_POOL , CChar
{- typedef struct clap_plugin_thread_pool {
            void (* exec)(const clap_plugin_t * plugin, uint32_t task_index);
        } clap_plugin_thread_pool_t; -}
#starttype struct clap_plugin_thread_pool
#field exec , FunPtr (Ptr <struct clap_plugin> -> CUInt -> IO ())
#stoptype
#synonym_t clap_plugin_thread_pool_t , <struct clap_plugin_thread_pool>
{- typedef struct clap_host_thread_pool {
            _Bool (* request_exec)(const clap_host_t * host,
                                   uint32_t num_tasks);
        } clap_host_thread_pool_t; -}
#starttype struct clap_host_thread_pool
#field request_exec , FunPtr (Ptr <struct clap_host> -> CUInt -> CInt)
#stoptype
#synonym_t clap_host_thread_pool_t , <struct clap_host_thread_pool>
