{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <clap/ext/audio-ports.h>
module Clap.Interface.Extension.Foreign.AudioPorts where
import Foreign.Ptr
#strict_import

import Clap.Interface.Foreign.Host
import Clap.Interface.Foreign.Plugin
#globalarray CLAP_EXT_AUDIO_PORTS , CChar
#globalarray CLAP_PORT_MONO , CChar
#globalarray CLAP_PORT_STEREO , CChar
{- enum {
    CLAP_AUDIO_PORT_IS_MAIN = 1 << 0,
    CLAP_AUDIO_PORT_SUPPORTS_64BITS = 1 << 1,
    CLAP_AUDIO_PORT_PREFERS_64BITS = 1 << 2,
    CLAP_AUDIO_PORT_REQUIRES_COMMON_SAMPLE_SIZE = 1 << 3
}; -}
#num CLAP_AUDIO_PORT_IS_MAIN
#num CLAP_AUDIO_PORT_SUPPORTS_64BITS
#num CLAP_AUDIO_PORT_PREFERS_64BITS
#num CLAP_AUDIO_PORT_REQUIRES_COMMON_SAMPLE_SIZE
{- typedef struct clap_audio_port_info {
            clap_id id;
            char name[CLAP_NAME_SIZE];
            uint32_t flags;
            uint32_t channel_count;
            const char * port_type;
            clap_id in_place_pair;
        } clap_audio_port_info_t; -}
#starttype struct clap_audio_port_info
#field id , CUInt
#array_field name , CChar
#field flags , CUInt
#field channel_count , CUInt
#field port_type , CString
#field in_place_pair , CUInt
#stoptype
#synonym_t clap_audio_port_info_t , <struct clap_audio_port_info>
{- typedef struct clap_plugin_audio_ports {
            uint32_t (* count)(const clap_plugin_t * plugin, _Bool is_input);
            _Bool (* get)(const clap_plugin_t * plugin,
                          uint32_t index,
                          _Bool is_input,
                          clap_audio_port_info_t * info);
        } clap_plugin_audio_ports_t; -}
#starttype struct clap_plugin_audio_ports
#field count , FunPtr (Ptr <struct clap_plugin> -> CInt -> CUInt)
#field get , FunPtr (Ptr <struct clap_plugin> -> CUInt -> CInt -> Ptr <struct clap_audio_port_info> -> CBool)
#stoptype
#synonym_t clap_plugin_audio_ports_t , <struct clap_plugin_audio_ports>
#callback_t count , Ptr <struct clap_plugin> -> CInt -> CUInt
#callback_t get , Ptr <struct clap_plugin> -> CUInt -> CInt -> Ptr <struct clap_audio_port_info> -> CBool
{- enum {
    CLAP_AUDIO_PORTS_RESCAN_NAMES = 1 << 0,
    CLAP_AUDIO_PORTS_RESCAN_FLAGS = 1 << 1,
    CLAP_AUDIO_PORTS_RESCAN_CHANNEL_COUNT = 1 << 2,
    CLAP_AUDIO_PORTS_RESCAN_PORT_TYPE = 1 << 3,
    CLAP_AUDIO_PORTS_RESCAN_IN_PLACE_PAIR = 1 << 4,
    CLAP_AUDIO_PORTS_RESCAN_LIST = 1 << 5
}; -}
#num CLAP_AUDIO_PORTS_RESCAN_NAMES
#num CLAP_AUDIO_PORTS_RESCAN_FLAGS
#num CLAP_AUDIO_PORTS_RESCAN_CHANNEL_COUNT
#num CLAP_AUDIO_PORTS_RESCAN_PORT_TYPE
#num CLAP_AUDIO_PORTS_RESCAN_IN_PLACE_PAIR
#num CLAP_AUDIO_PORTS_RESCAN_LIST
{- typedef struct clap_host_audio_ports {
            _Bool (* is_rescan_flag_supported)(const clap_host_t * host,
                                               uint32_t flag);
            void (* rescan)(const clap_host_t * host, uint32_t flags);
        } clap_host_audio_ports_t; -}
#starttype struct clap_host_audio_ports
#field is_rescan_flag_supported , FunPtr (Ptr <struct clap_host> -> CUInt -> CBool)
#field rescan , FunPtr (Ptr <struct clap_host> -> CUInt -> IO ())
#stoptype
#synonym_t clap_host_audio_ports_t , <struct clap_host_audio_ports>
#callback_t is_rescan_flag_supported , Ptr <struct clap_host> -> CUInt -> CBool
#callback_t rescan , Ptr <struct clap_host> -> CUInt -> IO ()