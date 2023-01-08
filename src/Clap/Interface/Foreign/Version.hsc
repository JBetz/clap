{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE CApiFFI #-}
#include <bindings.dsl.h>
#include <version.h>
module Clap.Interface.Foreign.Version where
import Foreign.Ptr
#strict_import

{- typedef struct clap_version {
            uint32_t major; uint32_t minor; uint32_t revision;
        } clap_version_t; -}
#starttype struct clap_version
#field major , CUInt
#field minor , CUInt
#field revision , CUInt
#stoptype
#synonym_t clap_version_t , <struct clap_version>
clapVersionMajor = #const CLAP_VERSION_MAJOR
clapVersionMinor = #const CLAP_VERSION_MINOR
clapVersionRevision = #const CLAP_VERSION_REVISION