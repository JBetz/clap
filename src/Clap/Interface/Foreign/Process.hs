{-# LINE 1 "src/Clap/Interface/Foreign/Process.hsc" #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}


module Clap.Interface.Foreign.Process where
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

{-# LINE 7 "src/Clap/Interface/Foreign/Process.hsc" #-}

import Clap.Interface.Foreign.Events
import Clap.Interface.Foreign.AudioBuffer

{- typedef int32_t clap_process_status; -}
type C'clap_process_status = CInt

{-# LINE 13 "src/Clap/Interface/Foreign/Process.hsc" #-}
{- typedef struct clap_process {
            int64_t steady_time;
            uint32_t frames_count;
            const clap_event_transport_t * transport;
            const clap_audio_buffer_t * audio_inputs;
            clap_audio_buffer_t * audio_outputs;
            uint32_t audio_inputs_count;
            uint32_t audio_outputs_count;
            const clap_input_events_t * in_events;
            const clap_output_events_t * out_events;
        } clap_process_t; -}

{-# LINE 25 "src/Clap/Interface/Foreign/Process.hsc" #-}

{-# LINE 26 "src/Clap/Interface/Foreign/Process.hsc" #-}

{-# LINE 27 "src/Clap/Interface/Foreign/Process.hsc" #-}

{-# LINE 28 "src/Clap/Interface/Foreign/Process.hsc" #-}

{-# LINE 29 "src/Clap/Interface/Foreign/Process.hsc" #-}

{-# LINE 30 "src/Clap/Interface/Foreign/Process.hsc" #-}

{-# LINE 31 "src/Clap/Interface/Foreign/Process.hsc" #-}

{-# LINE 32 "src/Clap/Interface/Foreign/Process.hsc" #-}

{-# LINE 33 "src/Clap/Interface/Foreign/Process.hsc" #-}

{-# LINE 34 "src/Clap/Interface/Foreign/Process.hsc" #-}
data C'clap_process = C'clap_process{
  c'clap_process'steady_time :: CLong,
  c'clap_process'frames_count :: CUInt,
  c'clap_process'transport :: Ptr C'clap_event_transport,
  c'clap_process'audio_inputs :: Ptr C'clap_audio_buffer,
  c'clap_process'audio_outputs :: Ptr C'clap_audio_buffer,
  c'clap_process'audio_inputs_count :: CUInt,
  c'clap_process'audio_outputs_count :: CUInt,
  c'clap_process'in_events :: Ptr C'clap_input_events,
  c'clap_process'out_events :: Ptr C'clap_output_events
} deriving (Eq,Show)
p'clap_process'steady_time p = plusPtr p 0
p'clap_process'steady_time :: Ptr (C'clap_process) -> Ptr (CLong)
p'clap_process'frames_count p = plusPtr p 8
p'clap_process'frames_count :: Ptr (C'clap_process) -> Ptr (CUInt)
p'clap_process'transport p = plusPtr p 16
p'clap_process'transport :: Ptr (C'clap_process) -> Ptr (Ptr C'clap_event_transport)
p'clap_process'audio_inputs p = plusPtr p 24
p'clap_process'audio_inputs :: Ptr (C'clap_process) -> Ptr (Ptr C'clap_audio_buffer)
p'clap_process'audio_outputs p = plusPtr p 32
p'clap_process'audio_outputs :: Ptr (C'clap_process) -> Ptr (Ptr C'clap_audio_buffer)
p'clap_process'audio_inputs_count p = plusPtr p 40
p'clap_process'audio_inputs_count :: Ptr (C'clap_process) -> Ptr (CUInt)
p'clap_process'audio_outputs_count p = plusPtr p 44
p'clap_process'audio_outputs_count :: Ptr (C'clap_process) -> Ptr (CUInt)
p'clap_process'in_events p = plusPtr p 48
p'clap_process'in_events :: Ptr (C'clap_process) -> Ptr (Ptr C'clap_input_events)
p'clap_process'out_events p = plusPtr p 56
p'clap_process'out_events :: Ptr (C'clap_process) -> Ptr (Ptr C'clap_output_events)
instance Storable C'clap_process where
  sizeOf _ = 64
  alignment _ = 8
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 8
    v2 <- peekByteOff _p 16
    v3 <- peekByteOff _p 24
    v4 <- peekByteOff _p 32
    v5 <- peekByteOff _p 40
    v6 <- peekByteOff _p 44
    v7 <- peekByteOff _p 48
    v8 <- peekByteOff _p 56
    return $ C'clap_process v0 v1 v2 v3 v4 v5 v6 v7 v8
  poke _p (C'clap_process v0 v1 v2 v3 v4 v5 v6 v7 v8) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 8 v1
    pokeByteOff _p 16 v2
    pokeByteOff _p 24 v3
    pokeByteOff _p 32 v4
    pokeByteOff _p 40 v5
    pokeByteOff _p 44 v6
    pokeByteOff _p 48 v7
    pokeByteOff _p 56 v8
    return ()

{-# LINE 35 "src/Clap/Interface/Foreign/Process.hsc" #-}
type C'clap_process_t = C'clap_process

{-# LINE 36 "src/Clap/Interface/Foreign/Process.hsc" #-}
