module Clap.Interface.AudioBuffer where

import Clap.Interface.Foreign.AudioBuffer
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Marshal.Utils

type AudioBufferHandle = Ptr C'clap_audio_buffer

data BufferData 
    = Data32 [[Float]]
    | Data64 [[Double]]

channelCount :: BufferData -> Int
channelCount (Data32 data32) = length data32
channelCount (Data64 data64) = length data64

createAudioBuffer :: BufferData -> Int -> Int -> IO AudioBufferHandle
createAudioBuffer bufferData latency constantMask = do
    (cData32, cData64) <- case bufferData of
            Data32 data32 -> do
                cData32 <- traverse newArray (fmap CFloat <$> data32) >>= newArray
                pure (cData32, nullPtr)
            Data64 data64 -> do
                cData64 <- traverse newArray (fmap CDouble <$> data64) >>= newArray
                pure (nullPtr, cData64) 
    cAudioBuffer <- new $ C'clap_audio_buffer
        { c'clap_audio_buffer'data32 = cData32
        , c'clap_audio_buffer'data64 = cData64
        , c'clap_audio_buffer'channel_count = fromIntegral $ channelCount bufferData
        , c'clap_audio_buffer'latency = fromIntegral latency
        , c'clap_audio_buffer'constant_mask = fromIntegral constantMask
        }
    pure cAudioBuffer