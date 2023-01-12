{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <clap/ext/voice-info.h>
module Clap.Interface.Extension.Foreign.VoiceInfo where
import Foreign.Ptr
#strict_import

import Clap.Interface.Extension.Foreign....Plugin
#globalarray CLAP_EXT_VOICE_INFO , CChar
{- enum {
    CLAP_VOICE_INFO_SUPPORTS_OVERLAPPING_NOTES = 1 << 0
}; -}
#num CLAP_VOICE_INFO_SUPPORTS_OVERLAPPING_NOTES
{- typedef struct clap_voice_info {
            uint32_t voice_count; uint32_t voice_capacity; uint64_t flags;
        } clap_voice_info_t; -}
#starttype struct clap_voice_info
#field voice_count , CUInt
#field voice_capacity , CUInt
#field flags , CULong
#stoptype
#synonym_t clap_voice_info_t , <struct clap_voice_info>
{- typedef struct clap_plugin_voice_info {
            _Bool (* get)(const clap_plugin_t * plugin,
                          clap_voice_info_t * info);
        } clap_plugin_voice_info_t; -}
#starttype struct clap_plugin_voice_info
#field get , FunPtr (Ptr <struct clap_plugin> -> Ptr <struct clap_voice_info> -> CInt)
#stoptype
#synonym_t clap_plugin_voice_info_t , <struct clap_plugin_voice_info>
{- typedef struct clap_host_voice_info {
            void (* changed)(const clap_host_t * host);
        } clap_host_voice_info_t; -}
#starttype struct clap_host_voice_info
#field changed , FunPtr (Ptr <struct clap_host> -> IO ())
#stoptype
#synonym_t clap_host_voice_info_t , <struct clap_host_voice_info>
