{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <clap/ext/note-name.h>
module Clap.Interface.Extension.Foreign.NoteName where
import Foreign.Ptr
#strict_import

import Clap.Interface.Extension.Foreign....Plugin
import Clap.Interface.Extension.Foreign....StringSizes
#globalarray CLAP_EXT_NOTE_NAME , CChar
{- typedef struct clap_note_name {
            char name[CLAP_NAME_SIZE];
            int16_t port;
            int16_t key;
            int16_t channel;
        } clap_note_name_t; -}
#starttype struct clap_note_name
#array_field name , CChar
#field port , CShort
#field key , CShort
#field channel , CShort
#stoptype
#synonym_t clap_note_name_t , <struct clap_note_name>
{- typedef struct clap_plugin_note_name {
            uint32_t (* count)(const clap_plugin_t * plugin);
            _Bool (* get)(const clap_plugin_t * plugin,
                          uint32_t index,
                          clap_note_name_t * note_name);
        } clap_plugin_note_name_t; -}
#starttype struct clap_plugin_note_name
#field count , FunPtr (Ptr <struct clap_plugin> -> CUInt)
#field get , FunPtr (Ptr <struct clap_plugin> -> CUInt -> Ptr <struct clap_note_name> -> CInt)
#stoptype
#synonym_t clap_plugin_note_name_t , <struct clap_plugin_note_name>
{- typedef struct clap_host_note_name {
            void (* changed)(const clap_host_t * host);
        } clap_host_note_name_t; -}
#starttype struct clap_host_note_name
#field changed , FunPtr (Ptr <struct clap_host> -> IO ())
#stoptype
#synonym_t clap_host_note_name_t , <struct clap_host_note_name>
