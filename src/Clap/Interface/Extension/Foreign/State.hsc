{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <clap/ext/state.h>
module Clap.Interface.Extension.Foreign.State where
import Foreign.Ptr
#strict_import

import Clap.Interface.Foreign.Host
import Clap.Interface.Foreign.Plugin
import Clap.Interface.Foreign.Stream
#globalarray CLAP_EXT_STATE , CChar
{- typedef struct clap_plugin_state {
            _Bool (* save)(const clap_plugin_t * plugin,
                           const clap_ostream_t * stream);
            _Bool (* load)(const clap_plugin_t * plugin,
                           const clap_istream_t * stream);
        } clap_plugin_state_t; -}
#starttype struct clap_plugin_state
#field save , FunPtr (Ptr <struct clap_plugin> -> Ptr <struct clap_ostream> -> CBool)
#field load , FunPtr (Ptr <struct clap_plugin> -> Ptr <struct clap_istream> -> CBool)
#stoptype
#synonym_t clap_plugin_state_t , <struct clap_plugin_state>
#callback_t save , Ptr <struct clap_plugin> -> Ptr <struct clap_ostream> -> CBool
#callback_t load , Ptr <struct clap_plugin> -> Ptr <struct clap_istream> -> CBool
{- typedef struct clap_host_state {
            void (* mark_dirty)(const clap_host_t * host);
        } clap_host_state_t; -}
#starttype struct clap_host_state
#field mark_dirty , FunPtr (Ptr <struct clap_host> -> IO ())
#stoptype
#synonym_t clap_host_state_t , <struct clap_host_state>
#callback_t mark_dirty , Ptr <struct clap_host> -> IO ()
