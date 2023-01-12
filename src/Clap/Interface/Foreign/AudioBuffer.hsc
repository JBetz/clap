{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <clap/audio-buffer.h>
module Clap.Interface.Foreign.AudioBuffer where
import Foreign.Ptr
#strict_import

{- typedef struct clap_audio_buffer {
            float * * data32;
            double * * data64;
            uint32_t channel_count;
            uint32_t latency;
            uint64_t constant_mask;
        } clap_audio_buffer_t; -}
#starttype struct clap_audio_buffer
#field data32 , Ptr (Ptr CFloat)
#field data64 , Ptr (Ptr CDouble)
#field channel_count , CUInt
#field latency , CUInt
#field constant_mask , CULong
#stoptype
#synonym_t clap_audio_buffer_t , <struct clap_audio_buffer>
