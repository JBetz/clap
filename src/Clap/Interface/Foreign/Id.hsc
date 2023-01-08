{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <id.h>
module Clap.Interface.Foreign.Id where
import Foreign.Ptr
#strict_import

{- typedef uint32_t clap_id; -}
#synonym_t clap_id , CUInt
