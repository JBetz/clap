{-# LINE 1 "src/Clap/Interface/Extension/Foreign/ThreadCheck.hsc" #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}


module Clap.Interface.Extension.Foreign.ThreadCheck where
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

{-# LINE 7 "src/Clap/Interface/Extension/Foreign/ThreadCheck.hsc" #-}

import Clap.Interface.Foreign.Host
import Clap.Interface.Foreign.Plugin
-- #globalarray CLAP_EXT_THREAD_CHECK , CChar
{- typedef struct clap_host_thread_check {
            _Bool (* is_main_thread)(const clap_host_t * host);
            _Bool (* is_audio_thread)(const clap_host_t * host);
        } clap_host_thread_check_t; -}

{-# LINE 16 "src/Clap/Interface/Extension/Foreign/ThreadCheck.hsc" #-}

{-# LINE 17 "src/Clap/Interface/Extension/Foreign/ThreadCheck.hsc" #-}

{-# LINE 18 "src/Clap/Interface/Extension/Foreign/ThreadCheck.hsc" #-}
data C'clap_host_thread_check = C'clap_host_thread_check{
  c'clap_host_thread_check'is_main_thread :: FunPtr (Ptr C'clap_host -> CBool),
  c'clap_host_thread_check'is_audio_thread :: FunPtr (Ptr C'clap_host -> CBool)
} deriving (Eq,Show)
p'clap_host_thread_check'is_main_thread p = plusPtr p 0
p'clap_host_thread_check'is_main_thread :: Ptr (C'clap_host_thread_check) -> Ptr (FunPtr (Ptr C'clap_host -> CBool))
p'clap_host_thread_check'is_audio_thread p = plusPtr p 8
p'clap_host_thread_check'is_audio_thread :: Ptr (C'clap_host_thread_check) -> Ptr (FunPtr (Ptr C'clap_host -> CBool))
instance Storable C'clap_host_thread_check where
  sizeOf _ = 16
  alignment _ = 8
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 8
    return $ C'clap_host_thread_check v0 v1
  poke _p (C'clap_host_thread_check v0 v1) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 8 v1
    return ()

{-# LINE 19 "src/Clap/Interface/Extension/Foreign/ThreadCheck.hsc" #-}
type C'clap_host_thread_check_t = C'clap_host_thread_check

{-# LINE 20 "src/Clap/Interface/Extension/Foreign/ThreadCheck.hsc" #-}
type C'is_main_thread = FunPtr (Ptr C'clap_host -> CBool)
foreign import ccall "wrapper" mk'is_main_thread
  :: (Ptr C'clap_host -> CBool) -> IO C'is_main_thread
foreign import ccall "dynamic" mK'is_main_thread
  :: C'is_main_thread -> (Ptr C'clap_host -> CBool)

{-# LINE 21 "src/Clap/Interface/Extension/Foreign/ThreadCheck.hsc" #-}
type C'is_audio_thread = FunPtr (Ptr C'clap_host -> CBool)
foreign import ccall "wrapper" mk'is_audio_thread
  :: (Ptr C'clap_host -> CBool) -> IO C'is_audio_thread
foreign import ccall "dynamic" mK'is_audio_thread
  :: C'is_audio_thread -> (Ptr C'clap_host -> CBool)
