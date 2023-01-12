{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <ext/render.h>
module Clap.Interface.Extension.Foreign.Render where
import Foreign.Ptr
#strict_import

import Clap.Interface.Foreign.Plugin
-- #globalarray CLAP_EXT_RENDER , CChar
{- enum {
    CLAP_RENDER_REALTIME = 0, CLAP_RENDER_OFFLINE = 1
}; -}
#num CLAP_RENDER_REALTIME
#num CLAP_RENDER_OFFLINE
{- typedef int32_t clap_plugin_render_mode; -}
#synonym_t clap_plugin_render_mode , CInt
{- typedef struct clap_plugin_render {
            _Bool (* has_hard_realtime_requirement)(const clap_plugin_t * plugin);
            _Bool (* set)(const clap_plugin_t * plugin,
                          clap_plugin_render_mode mode);
        } clap_plugin_render_t; -}
#starttype struct clap_plugin_render
#field has_hard_realtime_requirement , FunPtr (Ptr <struct clap_plugin> -> CBool)
#field set , FunPtr (Ptr <struct clap_plugin> -> CInt -> CBool)
#stoptype
#synonym_t clap_plugin_render_t , <struct clap_plugin_render>
#callback_t has_hard_realtime_requirement , Ptr <struct clap_plugin> -> CBool
#callback_t set , Ptr <struct clap_plugin> -> CInt -> CBool