{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <clap/plugin-invalidation.h>
module Clap.Interface.Foreign.PluginInvalidation where
import Foreign.Ptr
#strict_import

{- typedef struct clap_plugin_invalidation_source {
            const char * directory;
            const char * filename_glob;
            _Bool recursive_scan;
        } clap_plugin_invalidation_source_t; -}
#starttype struct clap_plugin_invalidation_source
#field directory , CString
#field filename_glob , CString
#field recursive_scan , CInt
#stoptype
#synonym_t clap_plugin_invalidation_source_t , <struct clap_plugin_invalidation_source>
#globalarray CLAP_PLUGIN_INVALIDATION_FACTORY_ID , CChar
{- typedef struct clap_plugin_invalidation_factory {
            uint32_t (* count)(const struct clap_plugin_invalidation_factory * factory);
            const clap_plugin_invalidation_source_t * (* get)(const struct clap_plugin_invalidation_factory * factory,
                                                              uint32_t index);
            _Bool (* refresh)(const struct clap_plugin_invalidation_factory * factory);
        } clap_plugin_invalidation_factory_t; -}
#starttype struct clap_plugin_invalidation_factory
#field count , FunPtr (Ptr <struct clap_plugin_invalidation_factory> -> CUInt)
#field get , FunPtr (Ptr <struct clap_plugin_invalidation_factory> -> CUInt -> Ptr <struct clap_plugin_invalidation_source>)
#field refresh , FunPtr (Ptr <struct clap_plugin_invalidation_factory> -> CInt)
#stoptype
#synonym_t clap_plugin_invalidation_factory_t , <struct clap_plugin_invalidation_factory>
#callback_t count , Ptr <struct clap_plugin_invalidation_factory> -> CUInt
#callback_t get , Ptr <struct clap_plugin_invalidation_factory> -> CUInt -> Ptr <struct clap_plugin_invalidation_source>
#callback_t refresh , Ptr <struct clap_plugin_invalidation_factory> -> CInt
