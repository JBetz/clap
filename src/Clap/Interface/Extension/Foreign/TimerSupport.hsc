{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <clap/ext/timer-support.h>
module Clap.Interface.Extension.Foreign.TimerSupport where
import Foreign.Ptr
#strict_import

import Clap.Interface.Extension.Foreign....Plugin
#globalarray CLAP_EXT_TIMER_SUPPORT , CChar
{- typedef struct clap_plugin_timer_support {
            void (* on_timer)(const clap_plugin_t * plugin, clap_id timer_id);
        } clap_plugin_timer_support_t; -}
#starttype struct clap_plugin_timer_support
#field on_timer , FunPtr (Ptr <struct clap_plugin> -> CUInt -> IO ())
#stoptype
#synonym_t clap_plugin_timer_support_t , <struct clap_plugin_timer_support>
{- typedef struct clap_host_timer_support {
            _Bool (* register_timer)(const clap_host_t * host,
                                     uint32_t period_ms,
                                     clap_id * timer_id);
            _Bool (* unregister_timer)(const clap_host_t * host,
                                       clap_id timer_id);
        } clap_host_timer_support_t; -}
#starttype struct clap_host_timer_support
#field register_timer , FunPtr (Ptr <struct clap_host> -> CUInt -> Ptr CUInt -> CInt)
#field unregister_timer , FunPtr (Ptr <struct clap_host> -> CUInt -> CInt)
#stoptype
#synonym_t clap_host_timer_support_t , <struct clap_host_timer_support>
