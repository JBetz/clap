{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <clap/ext/thread-check.h>
module Clap.Interface.Extension.Foreign.ThreadCheck where
import Foreign.Ptr
#strict_import

import Clap.Interface.Foreign.Host
import Clap.Interface.Foreign.Plugin
#globalarray CLAP_EXT_THREAD_CHECK , CChar
{- typedef struct clap_host_thread_check {
            _Bool (* is_main_thread)(const clap_host_t * host);
            _Bool (* is_audio_thread)(const clap_host_t * host);
        } clap_host_thread_check_t; -}
#starttype struct clap_host_thread_check
#field is_main_thread , FunPtr (Ptr <struct clap_host> -> CBool)
#field is_audio_thread , FunPtr (Ptr <struct clap_host> -> CBool)
#stoptype
#synonym_t clap_host_thread_check_t , <struct clap_host_thread_check>
#callback_t is_main_thread , Ptr <struct clap_host> -> CBool
#callback_t is_audio_thread , Ptr <struct clap_host> -> CBool