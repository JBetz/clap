{-# LANGUAGE GADTs #-}

module Clap.Interface.AudioBuffer where

import Clap.Interface.Foreign.AudioBuffer
import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Storable

type AudioBufferHandle = Ptr C'clap_audio_buffer

data BufferData t where
    Data32 :: Ptr (Ptr CFloat) -> BufferData CFloat
    Data64 :: Ptr (Ptr CDouble) -> BufferData CDouble

setBufferData :: AudioBufferHandle -> BufferData t -> IO () 
setBufferData audioBuffer bufferData =
    case bufferData of
        Data32 data32 -> do
            poke (p'clap_audio_buffer'data32 audioBuffer) data32
            poke (p'clap_audio_buffer'data64 audioBuffer) nullPtr
        Data64 data64 -> do
            poke (p'clap_audio_buffer'data32 audioBuffer) nullPtr
            poke (p'clap_audio_buffer'data64 audioBuffer) data64


setChannelCount :: AudioBufferHandle -> Word32 -> IO ()
setChannelCount audioBuffer =
    poke (p'clap_audio_buffer'channel_count audioBuffer) . fromIntegral

setConstantMask :: AudioBufferHandle -> Word32 -> IO ()
setConstantMask audioBuffer =
    poke (p'clap_audio_buffer'constant_mask audioBuffer) . fromIntegral

setLatency :: AudioBufferHandle -> Word64 -> IO ()
setLatency audioBuffer =
    poke (p'clap_audio_buffer'latency audioBuffer) . fromIntegral