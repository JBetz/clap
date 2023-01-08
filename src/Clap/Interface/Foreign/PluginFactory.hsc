{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include "clap.h"
module Clap.Interface.Foreign.PluginFactory where
import Foreign.Ptr
#strict_import

import Clap.Interface.Foreign.Host
import Clap.Interface.Foreign.Plugin
{- typedef struct clap_plugin_factory {
            uint32_t (* get_plugin_count)(const struct clap_plugin_factory * factory);
            const clap_plugin_descriptor_t * (* get_plugin_descriptor)(const struct clap_plugin_factory * factory,
                                                                       uint32_t index);
            const clap_plugin_t * (* create_plugin)(const struct clap_plugin_factory * factory,
                                                    const clap_host_t * host,
                                                    const char * plugin_id);
        } clap_plugin_factory_t; -}
#starttype struct clap_plugin_factory
#field get_plugin_count , FunPtr (Ptr <struct clap_plugin_factory> -> CUInt)
#field get_plugin_descriptor , FunPtr (Ptr <struct clap_plugin_factory> -> CUInt -> Ptr <struct clap_plugin_descriptor>)
#field create_plugin , FunPtr (Ptr <struct clap_plugin_factory> -> Ptr <struct clap_host> -> CString -> Ptr <struct clap_plugin>)
#stoptype
#synonym_t clap_plugin_factory_t , <struct clap_plugin_factory>
#callback_t get_plugin_count , Ptr <struct clap_plugin_factory> -> CUInt
#callback_t get_plugin_descriptor , Ptr <struct clap_plugin_factory> -> CUInt -> Ptr <struct clap_plugin_descriptor>
#callback_t create_plugin , Ptr <struct clap_plugin_factory> -> Ptr <struct clap_host> -> CString -> Ptr <struct clap_plugin>