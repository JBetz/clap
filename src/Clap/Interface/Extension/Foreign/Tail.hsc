{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <clap/ext/tail.h>
module Clap.Interface.Extension.Foreign.Tail where
import Foreign.Ptr
#strict_import

import Clap.Interface.Extension.Foreign....Plugin
#globalarray CLAP_EXT_TAIL , CChar
{- typedef struct clap_plugin_tail {
            uint32_t (* get)(const clap_plugin_t * plugin);
        } clap_plugin_tail_t; -}
#starttype struct clap_plugin_tail
#field get , FunPtr (Ptr <struct clap_plugin> -> CUInt)
#stoptype
#synonym_t clap_plugin_tail_t , <struct clap_plugin_tail>
{- typedef struct clap_host_tail {
            void (* changed)(const clap_host_t * host);
        } clap_host_tail_t; -}
#starttype struct clap_host_tail
#field changed , FunPtr (Ptr <struct clap_host> -> IO ())
#stoptype
#synonym_t clap_host_tail_t , <struct clap_host_tail>
