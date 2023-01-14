{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <clap/ext/note-ports.h>
module Clap.Interface.Extension.Foreign.NotePorts where
import Foreign.Ptr
#strict_import

import Clap.Interface.Foreign.Host
import Clap.Interface.Foreign.Plugin
import Clap.Interface.StringSizes
#globalarray CLAP_EXT_NOTE_PORTS , CChar
{- enum clap_note_dialect {
    CLAP_NOTE_DIALECT_CLAP = 1 << 0,
    CLAP_NOTE_DIALECT_MIDI = 1 << 1,
    CLAP_NOTE_DIALECT_MIDI_MPE = 1 << 2,
    CLAP_NOTE_DIALECT_MIDI2 = 1 << 3
}; -}
#integral_t enum clap_note_dialect
#num CLAP_NOTE_DIALECT_CLAP
#num CLAP_NOTE_DIALECT_MIDI
#num CLAP_NOTE_DIALECT_MIDI_MPE
#num CLAP_NOTE_DIALECT_MIDI2
{- typedef struct clap_note_port_info {
            clap_id id;
            uint32_t supported_dialects;
            uint32_t preferred_dialect;
            char name[CLAP_NAME_SIZE];
        } clap_note_port_info_t; -}
#starttype struct clap_note_port_info
#field id , CUInt
#field supported_dialects , CUInt
#field preferred_dialect , CUInt
#array_field name , CChar
#stoptype
#synonym_t clap_note_port_info_t , <struct clap_note_port_info>
{- typedef struct clap_plugin_note_ports {
            uint32_t (* count)(const clap_plugin_t * plugin, _Bool is_input);
            _Bool (* get)(const clap_plugin_t * plugin,
                          uint32_t index,
                          _Bool is_input,
                          clap_note_port_info_t * info);
        } clap_plugin_note_ports_t; -}
#starttype struct clap_plugin_note_ports
#field count , FunPtr (Ptr <struct clap_plugin> -> CBool -> CUInt)
#field get , FunPtr (Ptr <struct clap_plugin> -> CUInt -> CInt -> Ptr <struct clap_note_port_info> -> CBool)
#stoptype
#synonym_t clap_plugin_note_ports_t , <struct clap_plugin_note_ports>
#callback_t count , Ptr <struct clap_plugin> -> CBool -> CUInt
#callback_t get , Ptr <struct clap_plugin> -> CUInt -> CInt -> Ptr <struct clap_note_port_info> -> CBool
{- enum {
    CLAP_NOTE_PORTS_RESCAN_ALL = 1 << 0,
    CLAP_NOTE_PORTS_RESCAN_NAMES = 1 << 1
}; -}
#num CLAP_NOTE_PORTS_RESCAN_ALL
#num CLAP_NOTE_PORTS_RESCAN_NAMES
{- typedef struct clap_host_note_ports {
            uint32_t (* supported_dialects)(const clap_host_t * host);
            void (* rescan)(const clap_host_t * host, uint32_t flags);
        } clap_host_note_ports_t; -}
#starttype struct clap_host_note_ports
#field supported_dialects , FunPtr (Ptr <struct clap_host> -> CUInt)
#field rescan , FunPtr (Ptr <struct clap_host> -> CUInt -> IO ())
#stoptype
#synonym_t clap_host_note_ports_t , <struct clap_host_note_ports>
#callback_t supported_dialects , Ptr <struct clap_host> -> CUInt
#callback_t rescan , Ptr <struct clap_host> -> CUInt -> IO ()