{-# LINE 1 "src/Clap/Interface/Foreign/AudioBuffer.hsc" #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}


module Clap.Interface.Foreign.AudioBuffer where
import Foreign.Ptr
import Foreign.Ptr (Ptr,FunPtr,plusPtr)
import Foreign.Ptr (wordPtrToPtr,castPtrToFunPtr)
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String (CString,CStringLen,CWString,CWStringLen)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (peekArray,pokeArray)
import Data.Int
import Data.Word

{-# LINE 7 "src/Clap/Interface/Foreign/AudioBuffer.hsc" #-}

{- typedef struct clap_audio_buffer {
            float * * data32;
            double * * data64;
            uint32_t channel_count;
            uint32_t latency;
            uint64_t constant_mask;
        } clap_audio_buffer_t; -}

{-# LINE 16 "src/Clap/Interface/Foreign/AudioBuffer.hsc" #-}

{-# LINE 17 "src/Clap/Interface/Foreign/AudioBuffer.hsc" #-}

{-# LINE 18 "src/Clap/Interface/Foreign/AudioBuffer.hsc" #-}

{-# LINE 19 "src/Clap/Interface/Foreign/AudioBuffer.hsc" #-}

{-# LINE 20 "src/Clap/Interface/Foreign/AudioBuffer.hsc" #-}

{-# LINE 21 "src/Clap/Interface/Foreign/AudioBuffer.hsc" #-}
data C'clap_audio_buffer = C'clap_audio_buffer{
  c'clap_audio_buffer'data32 :: Ptr (Ptr CFloat),
  c'clap_audio_buffer'data64 :: Ptr (Ptr CDouble),
  c'clap_audio_buffer'channel_count :: CUInt,
  c'clap_audio_buffer'latency :: CUInt,
  c'clap_audio_buffer'constant_mask :: CULong
} deriving (Eq,Show)
p'clap_audio_buffer'data32 p = plusPtr p 0
p'clap_audio_buffer'data32 :: Ptr (C'clap_audio_buffer) -> Ptr (Ptr (Ptr CFloat))
p'clap_audio_buffer'data64 p = plusPtr p 8
p'clap_audio_buffer'data64 :: Ptr (C'clap_audio_buffer) -> Ptr (Ptr (Ptr CDouble))
p'clap_audio_buffer'channel_count p = plusPtr p 16
p'clap_audio_buffer'channel_count :: Ptr (C'clap_audio_buffer) -> Ptr (CUInt)
p'clap_audio_buffer'latency p = plusPtr p 20
p'clap_audio_buffer'latency :: Ptr (C'clap_audio_buffer) -> Ptr (CUInt)
p'clap_audio_buffer'constant_mask p = plusPtr p 24
p'clap_audio_buffer'constant_mask :: Ptr (C'clap_audio_buffer) -> Ptr (CULong)
instance Storable C'clap_audio_buffer where
  sizeOf _ = 32
  alignment _ = 8
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 8
    v2 <- peekByteOff _p 16
    v3 <- peekByteOff _p 20
    v4 <- peekByteOff _p 24
    return $ C'clap_audio_buffer v0 v1 v2 v3 v4
  poke _p (C'clap_audio_buffer v0 v1 v2 v3 v4) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 8 v1
    pokeByteOff _p 16 v2
    pokeByteOff _p 20 v3
    pokeByteOff _p 24 v4
    return ()

{-# LINE 22 "src/Clap/Interface/Foreign/AudioBuffer.hsc" #-}
type C'clap_audio_buffer_t = C'clap_audio_buffer

{-# LINE 23 "src/Clap/Interface/Foreign/AudioBuffer.hsc" #-}
