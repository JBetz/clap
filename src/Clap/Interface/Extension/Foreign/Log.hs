{-# LINE 1 "src/Clap/Interface/Extension/Foreign/Log.hsc" #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}


module Clap.Interface.Extension.Foreign.Log where
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

{-# LINE 7 "src/Clap/Interface/Extension/Foreign/Log.hsc" #-}

import Clap.Interface.Foreign.Plugin
import Clap.Interface.Foreign.Host

-- #globalarray CLAP_EXT_LOG , CChar
{- enum {
    CLAP_LOG_DEBUG = 0,
    CLAP_LOG_INFO = 1,
    CLAP_LOG_WARNING = 2,
    CLAP_LOG_ERROR = 3,
    CLAP_LOG_FATAL = 4,
    CLAP_LOG_HOST_MISBEHAVING = 5,
    CLAP_LOG_PLUGIN_MISBEHAVING = 6
}; -}
c'CLAP_LOG_DEBUG = 0
c'CLAP_LOG_DEBUG :: (Num a) => a

{-# LINE 22 "src/Clap/Interface/Extension/Foreign/Log.hsc" #-}
c'CLAP_LOG_INFO = 1
c'CLAP_LOG_INFO :: (Num a) => a

{-# LINE 23 "src/Clap/Interface/Extension/Foreign/Log.hsc" #-}
c'CLAP_LOG_WARNING = 2
c'CLAP_LOG_WARNING :: (Num a) => a

{-# LINE 24 "src/Clap/Interface/Extension/Foreign/Log.hsc" #-}
c'CLAP_LOG_ERROR = 3
c'CLAP_LOG_ERROR :: (Num a) => a

{-# LINE 25 "src/Clap/Interface/Extension/Foreign/Log.hsc" #-}
c'CLAP_LOG_FATAL = 4
c'CLAP_LOG_FATAL :: (Num a) => a

{-# LINE 26 "src/Clap/Interface/Extension/Foreign/Log.hsc" #-}
c'CLAP_LOG_HOST_MISBEHAVING = 5
c'CLAP_LOG_HOST_MISBEHAVING :: (Num a) => a

{-# LINE 27 "src/Clap/Interface/Extension/Foreign/Log.hsc" #-}
c'CLAP_LOG_PLUGIN_MISBEHAVING = 6
c'CLAP_LOG_PLUGIN_MISBEHAVING :: (Num a) => a

{-# LINE 28 "src/Clap/Interface/Extension/Foreign/Log.hsc" #-}
{- typedef int32_t clap_log_severity; -}
type C'clap_log_severity = CInt

{-# LINE 30 "src/Clap/Interface/Extension/Foreign/Log.hsc" #-}
{- typedef struct clap_host_log {
            void (* log)(const clap_host_t * host,
                         clap_log_severity severity,
                         const char * msg);
        } clap_host_log_t; -}

{-# LINE 36 "src/Clap/Interface/Extension/Foreign/Log.hsc" #-}

{-# LINE 37 "src/Clap/Interface/Extension/Foreign/Log.hsc" #-}
data C'clap_host_log = C'clap_host_log{
  c'clap_host_log'log :: FunPtr (Ptr C'clap_host -> CInt -> CString -> IO ())
} deriving (Eq,Show)
p'clap_host_log'log p = plusPtr p 0
p'clap_host_log'log :: Ptr (C'clap_host_log) -> Ptr (FunPtr (Ptr C'clap_host -> CInt -> CString -> IO ()))
instance Storable C'clap_host_log where
  sizeOf _ = 8
  alignment _ = 8
  peek _p = do
    v0 <- peekByteOff _p 0
    return $ C'clap_host_log v0
  poke _p (C'clap_host_log v0) = do
    pokeByteOff _p 0 v0
    return ()

{-# LINE 38 "src/Clap/Interface/Extension/Foreign/Log.hsc" #-}
type C'clap_host_log_t = C'clap_host_log

{-# LINE 39 "src/Clap/Interface/Extension/Foreign/Log.hsc" #-}
type C'log = FunPtr (Ptr C'clap_host -> CInt -> CString -> IO ())
foreign import ccall "wrapper" mk'log
  :: (Ptr C'clap_host -> CInt -> CString -> IO ()) -> IO C'log
foreign import ccall "dynamic" mK'log
  :: C'log -> (Ptr C'clap_host -> CInt -> CString -> IO ())
