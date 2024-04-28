{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <clap/color.h>
module Clap.Interface.Foreign.Color where
import Foreign.Ptr
#strict_import

{- typedef struct clap_color {
            uint8_t alpha; uint8_t red; uint8_t green; uint8_t blue;
        } clap_color_t; -}
#starttype struct clap_color
#field alpha , CUChar
#field red , CUChar
#field green , CUChar
#field blue , CUChar
#stoptype
#synonym_t clap_color_t , <struct clap_color>
