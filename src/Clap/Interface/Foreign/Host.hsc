{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <host.h>
module Clap.Interface.Foreign.Host where
import Foreign.Ptr
#strict_import

import Clap.Interface.Foreign.Version
{- typedef struct clap_host {
            clap_version_t clap_version;
            void * host_data;
            const char * name;
            const char * vendor;
            const char * url;
            const char * version;
            const void * (* get_extension)(const struct clap_host * host,
                                           const char * extension_id);
            void (* request_restart)(const struct clap_host * host);
            void (* request_process)(const struct clap_host * host);
            void (* request_callback)(const struct clap_host * host);
        } clap_host_t; -}
#starttype struct clap_host
#field clap_version , <struct clap_version>
#field host_data , Ptr ()
#field name , CString
#field vendor , CString
#field url , CString
#field version , CString
#field get_extension , FunPtr (Ptr <struct clap_host> -> CString -> IO (Ptr ()))
#field request_restart , FunPtr (Ptr <struct clap_host> -> IO ())
#field request_process , FunPtr (Ptr <struct clap_host> -> IO ())
#field request_callback , FunPtr (Ptr <struct clap_host> -> IO ())
#stoptype
#synonym_t clap_host_t , <struct clap_host>
#callback_t get_extension , Ptr <struct clap_host> -> CString -> IO (Ptr ())
#callback_t request_restart , Ptr <struct clap_host> -> IO ()
#callback_t request_process , Ptr <struct clap_host> -> IO ()
#callback_t request_callback , Ptr <struct clap_host> -> IO ()