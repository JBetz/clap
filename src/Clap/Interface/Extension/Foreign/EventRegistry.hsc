{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include "include/ext/event-registry.h"
module Clap.Interface.Extension.Foreign.EventRegistry where
import Foreign.Ptr
#strict_import

import Clap.Interface.Extension.Foreign....Plugin
#globalarray CLAP_EXT_EVENT_REGISTRY , CChar
{- typedef struct clap_host_event_registry {
            _Bool (* query)(const clap_host_t * host,
                            const char * space_name,
                            uint16_t * space_id);
        } clap_host_event_registry_t; -}
#starttype struct clap_host_event_registry
#field query , FunPtr (Ptr <struct clap_host> -> CString -> Ptr CUShort -> CInt)
#stoptype
#synonym_t clap_host_event_registry_t , <struct clap_host_event_registry>
