{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <clap/fixedpoint.h>
module Clap.Interface.Foreign.Fixedpoint where
import Foreign.Ptr
#strict_import

{- typedef int64_t clap_beattime; -}
#synonym_t clap_beattime , CLong
{- typedef int64_t clap_sectime; -}
#synonym_t clap_sectime , CLong
