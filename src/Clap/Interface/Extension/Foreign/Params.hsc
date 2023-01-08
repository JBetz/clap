{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include "include/ext/params.h"
module Clap.Interface.Extension.Foreign.Params where
import Foreign.Ptr
#strict_import

import Clap.Interface.Extension.Foreign....Plugin
import Clap.Interface.Extension.Foreign....StringSizes
#globalarray CLAP_EXT_PARAMS , CChar
{- enum {
    CLAP_PARAM_IS_STEPPED = 1 << 0,
    CLAP_PARAM_IS_PERIODIC = 1 << 1,
    CLAP_PARAM_IS_HIDDEN = 1 << 2,
    CLAP_PARAM_IS_READONLY = 1 << 3,
    CLAP_PARAM_IS_BYPASS = 1 << 4,
    CLAP_PARAM_IS_AUTOMATABLE = 1 << 5,
    CLAP_PARAM_IS_AUTOMATABLE_PER_NOTE_ID = 1 << 6,
    CLAP_PARAM_IS_AUTOMATABLE_PER_KEY = 1 << 7,
    CLAP_PARAM_IS_AUTOMATABLE_PER_CHANNEL = 1 << 8,
    CLAP_PARAM_IS_AUTOMATABLE_PER_PORT = 1 << 9,
    CLAP_PARAM_IS_MODULATABLE = 1 << 10,
    CLAP_PARAM_IS_MODULATABLE_PER_NOTE_ID = 1 << 11,
    CLAP_PARAM_IS_MODULATABLE_PER_KEY = 1 << 12,
    CLAP_PARAM_IS_MODULATABLE_PER_CHANNEL = 1 << 13,
    CLAP_PARAM_IS_MODULATABLE_PER_PORT = 1 << 14,
    CLAP_PARAM_REQUIRES_PROCESS = 1 << 15
}; -}
#num CLAP_PARAM_IS_STEPPED
#num CLAP_PARAM_IS_PERIODIC
#num CLAP_PARAM_IS_HIDDEN
#num CLAP_PARAM_IS_READONLY
#num CLAP_PARAM_IS_BYPASS
#num CLAP_PARAM_IS_AUTOMATABLE
#num CLAP_PARAM_IS_AUTOMATABLE_PER_NOTE_ID
#num CLAP_PARAM_IS_AUTOMATABLE_PER_KEY
#num CLAP_PARAM_IS_AUTOMATABLE_PER_CHANNEL
#num CLAP_PARAM_IS_AUTOMATABLE_PER_PORT
#num CLAP_PARAM_IS_MODULATABLE
#num CLAP_PARAM_IS_MODULATABLE_PER_NOTE_ID
#num CLAP_PARAM_IS_MODULATABLE_PER_KEY
#num CLAP_PARAM_IS_MODULATABLE_PER_CHANNEL
#num CLAP_PARAM_IS_MODULATABLE_PER_PORT
#num CLAP_PARAM_REQUIRES_PROCESS
{- typedef uint32_t clap_param_info_flags; -}
#synonym_t clap_param_info_flags , CUInt
{- typedef struct clap_param_info {
            clap_id id;
            clap_param_info_flags flags;
            void * cookie;
            char name[CLAP_NAME_SIZE];
            char module[CLAP_PATH_SIZE];
            double min_value;
            double max_value;
            double default_value;
        } clap_param_info_t; -}
#starttype struct clap_param_info
#field id , CUInt
#field flags , CUInt
#field cookie , Ptr ()
#array_field name , CChar
#array_field module , CChar
#field min_value , CDouble
#field max_value , CDouble
#field default_value , CDouble
#stoptype
#synonym_t clap_param_info_t , <struct clap_param_info>
{- typedef struct clap_plugin_params {
            uint32_t (* count)(const clap_plugin_t * plugin);
            _Bool (* get_info)(const clap_plugin_t * plugin,
                               uint32_t param_index,
                               clap_param_info_t * param_info);
            _Bool (* get_value)(const clap_plugin_t * plugin,
                                clap_id param_id,
                                double * out_value);
            _Bool (* value_to_text)(const clap_plugin_t * plugin,
                                    clap_id param_id,
                                    double value,
                                    char * out_buffer,
                                    uint32_t out_buffer_capacity);
            _Bool (* text_to_value)(const clap_plugin_t * plugin,
                                    clap_id param_id,
                                    const char * param_value_text,
                                    double * out_value);
            void (* flush)(const clap_plugin_t * plugin,
                           const clap_input_events_t * in,
                           const clap_output_events_t * out);
        } clap_plugin_params_t; -}
#starttype struct clap_plugin_params
#field count , FunPtr (Ptr <struct clap_plugin> -> CUInt)
#field get_info , FunPtr (Ptr <struct clap_plugin> -> CUInt -> Ptr <struct clap_param_info> -> CInt)
#field get_value , FunPtr (Ptr <struct clap_plugin> -> CUInt -> Ptr CDouble -> CInt)
#field value_to_text , FunPtr (Ptr <struct clap_plugin> -> CUInt -> CDouble -> CString -> CUInt -> CInt)
#field text_to_value , FunPtr (Ptr <struct clap_plugin> -> CUInt -> CString -> Ptr CDouble -> CInt)
#field flush , FunPtr (Ptr <struct clap_plugin> -> Ptr <struct clap_input_events> -> Ptr <struct clap_output_events> -> IO ())
#stoptype
#synonym_t clap_plugin_params_t , <struct clap_plugin_params>
{- enum {
    CLAP_PARAM_RESCAN_VALUES = 1 << 0,
    CLAP_PARAM_RESCAN_TEXT = 1 << 1,
    CLAP_PARAM_RESCAN_INFO = 1 << 2,
    CLAP_PARAM_RESCAN_ALL = 1 << 3
}; -}
#num CLAP_PARAM_RESCAN_VALUES
#num CLAP_PARAM_RESCAN_TEXT
#num CLAP_PARAM_RESCAN_INFO
#num CLAP_PARAM_RESCAN_ALL
{- typedef uint32_t clap_param_rescan_flags; -}
#synonym_t clap_param_rescan_flags , CUInt
{- enum {
    CLAP_PARAM_CLEAR_ALL = 1 << 0,
    CLAP_PARAM_CLEAR_AUTOMATIONS = 1 << 1,
    CLAP_PARAM_CLEAR_MODULATIONS = 1 << 2
}; -}
#num CLAP_PARAM_CLEAR_ALL
#num CLAP_PARAM_CLEAR_AUTOMATIONS
#num CLAP_PARAM_CLEAR_MODULATIONS
{- typedef uint32_t clap_param_clear_flags; -}
#synonym_t clap_param_clear_flags , CUInt
{- typedef struct clap_host_params {
            void (* rescan)(const clap_host_t * host,
                            clap_param_rescan_flags flags);
            void (* clear)(const clap_host_t * host,
                           clap_id param_id,
                           clap_param_clear_flags flags);
            void (* request_flush)(const clap_host_t * host);
        } clap_host_params_t; -}
#starttype struct clap_host_params
#field rescan , FunPtr (Ptr <struct clap_host> -> CUInt -> IO ())
#field clear , FunPtr (Ptr <struct clap_host> -> CUInt -> CUInt -> IO ())
#field request_flush , FunPtr (Ptr <struct clap_host> -> IO ())
#stoptype
#synonym_t clap_host_params_t , <struct clap_host_params>
