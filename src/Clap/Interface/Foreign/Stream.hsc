{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <clap/stream.h>
module Clap.Stream where
import Foreign.Ptr
#strict_import

import Clap.Private.Std
import Clap.Private.Macros
{- typedef struct clap_istream {
            void * ctx;
            int64_t (* read)(const struct clap_istream * stream,
                             void * buffer,
                             uint64_t size);
        } clap_istream_t; -}
#starttype struct clap_istream
#field ctx , Ptr ()
#field read , FunPtr (Ptr <struct clap_istream> -> Ptr () -> CULong -> CLong)
#stoptype
#synonym_t clap_istream_t , <struct clap_istream>
{- typedef struct clap_ostream {
            void * ctx;
            int64_t (* write)(const struct clap_ostream * stream,
                              const void * buffer,
                              uint64_t size);
        } clap_ostream_t; -}
#starttype struct clap_ostream
#field ctx , Ptr ()
#field write , FunPtr (Ptr <struct clap_ostream> -> Ptr () -> CULong -> CLong)
#stoptype
#synonym_t clap_ostream_t , <struct clap_ostream>
