{-# OPTIONS_GHC -fno-warn-unused-imports #-}

#include <bindings.dsl.h>
#include <clap/entry.h>

module Clap.Interface.Foreign.Entry where

import Foreign.Ptr
#strict_import

import Clap.Interface.Foreign.Version
{- typedef struct clap_plugin_entry {
            clap_version_t clap_version;
            _Bool (* init)(const char * plugin_path);
            void (* deinit)(void);
            const void * (* get_factory)(const char * factory_id);
        } clap_plugin_entry_t; -}
#starttype struct clap_plugin_entry
#field clap_version , <struct clap_version>
#field init , FunPtr (CString -> CBool)
#field deinit , FunPtr (IO ())
#field get_factory , FunPtr (CString -> Ptr ())
#stoptype
#synonym_t clap_plugin_entry_t , <struct clap_plugin_entry>
#callback_t init , CString -> CBool
#callback_t deinit , IO ()
#callback_t get_factory , CString -> Ptr ()