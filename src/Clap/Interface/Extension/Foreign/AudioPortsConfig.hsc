{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <clap/ext/audio-ports-config.h>
module Clap.Interface.Extension.Foreign.AudioPortsConfig where
import Foreign.Ptr
#strict_import

import Clap.Interface.Extension.Foreign....StringSizes
import Clap.Interface.Extension.Foreign....Plugin
#globalarray CLAP_EXT_AUDIO_PORTS_CONFIG , CChar
{- typedef struct clap_audio_ports_config {
            clap_id id;
            char name[CLAP_NAME_SIZE];
            uint32_t input_port_count;
            uint32_t output_port_count;
            _Bool has_main_input;
            uint32_t main_input_channel_count;
            const char * main_input_port_type;
            _Bool has_main_output;
            uint32_t main_output_channel_count;
            const char * main_output_port_type;
        } clap_audio_ports_config_t; -}
#starttype struct clap_audio_ports_config
#field id , CUInt
#array_field name , CChar
#field input_port_count , CUInt
#field output_port_count , CUInt
#field has_main_input , CInt
#field main_input_channel_count , CUInt
#field main_input_port_type , CString
#field has_main_output , CInt
#field main_output_channel_count , CUInt
#field main_output_port_type , CString
#stoptype
#synonym_t clap_audio_ports_config_t , <struct clap_audio_ports_config>
{- typedef struct clap_plugin_audio_ports_config {
            uint32_t (* count)(const clap_plugin_t * plugin);
            _Bool (* get)(const clap_plugin_t * plugin,
                          uint32_t index,
                          clap_audio_ports_config_t * config);
            _Bool (* select)(const clap_plugin_t * plugin, clap_id config_id);
        } clap_plugin_audio_ports_config_t; -}
#starttype struct clap_plugin_audio_ports_config
#field count , FunPtr (Ptr <struct clap_plugin> -> CUInt)
#field get , FunPtr (Ptr <struct clap_plugin> -> CUInt -> Ptr <struct clap_audio_ports_config> -> CInt)
#field select , FunPtr (Ptr <struct clap_plugin> -> CUInt -> CInt)
#stoptype
#synonym_t clap_plugin_audio_ports_config_t , <struct clap_plugin_audio_ports_config>
{- typedef struct clap_host_audio_ports_config {
            void (* rescan)(const clap_host_t * host);
        } clap_host_audio_ports_config_t; -}
#starttype struct clap_host_audio_ports_config
#field rescan , FunPtr (Ptr <struct clap_host> -> IO ())
#stoptype
#synonym_t clap_host_audio_ports_config_t , <struct clap_host_audio_ports_config>
