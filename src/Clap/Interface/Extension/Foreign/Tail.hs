{-# LINE 1 "src/Clap/Interface/Extension/Foreign/Tail.hsc" #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}


module Clap.Interface.Extension.Foreign.Tail where
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

{-# LINE 7 "src/Clap/Interface/Extension/Foreign/Tail.hsc" #-}

import Clap.Interface.Foreign.Host
import Clap.Interface.Foreign.Plugin
-- #globalarray CLAP_EXT_TAIL , CChar
{- typedef struct clap_plugin_tail {
            uint32_t (* get)(const clap_plugin_t * plugin);
        } clap_plugin_tail_t; -}

{-# LINE 15 "src/Clap/Interface/Extension/Foreign/Tail.hsc" #-}

{-# LINE 16 "src/Clap/Interface/Extension/Foreign/Tail.hsc" #-}
data C'clap_plugin_tail = C'clap_plugin_tail{
  c'clap_plugin_tail'get :: FunPtr (Ptr C'clap_plugin -> CUInt)
} deriving (Eq,Show)
p'clap_plugin_tail'get p = plusPtr p 0
p'clap_plugin_tail'get :: Ptr (C'clap_plugin_tail) -> Ptr (FunPtr (Ptr C'clap_plugin -> CUInt))
instance Storable C'clap_plugin_tail where
  sizeOf _ = 8
  alignment _ = 8
  peek _p = do
    v0 <- peekByteOff _p 0
    return $ C'clap_plugin_tail v0
  poke _p (C'clap_plugin_tail v0) = do
    pokeByteOff _p 0 v0
    return ()

{-# LINE 17 "src/Clap/Interface/Extension/Foreign/Tail.hsc" #-}
type C'clap_plugin_tail_t = C'clap_plugin_tail

{-# LINE 18 "src/Clap/Interface/Extension/Foreign/Tail.hsc" #-}
type C'get = FunPtr (Ptr C'clap_plugin -> CUInt)
foreign import ccall "wrapper" mk'get
  :: (Ptr C'clap_plugin -> CUInt) -> IO C'get
foreign import ccall "dynamic" mK'get
  :: C'get -> (Ptr C'clap_plugin -> CUInt)

{-# LINE 19 "src/Clap/Interface/Extension/Foreign/Tail.hsc" #-}
{- typedef struct clap_host_tail {
            void (* changed)(const clap_host_t * host);
        } clap_host_tail_t; -}

{-# LINE 23 "src/Clap/Interface/Extension/Foreign/Tail.hsc" #-}

{-# LINE 24 "src/Clap/Interface/Extension/Foreign/Tail.hsc" #-}
data C'clap_host_tail = C'clap_host_tail{
  c'clap_host_tail'changed :: FunPtr (Ptr C'clap_host -> IO ())
} deriving (Eq,Show)
p'clap_host_tail'changed p = plusPtr p 0
p'clap_host_tail'changed :: Ptr (C'clap_host_tail) -> Ptr (FunPtr (Ptr C'clap_host -> IO ()))
instance Storable C'clap_host_tail where
  sizeOf _ = 8
  alignment _ = 8
  peek _p = do
    v0 <- peekByteOff _p 0
    return $ C'clap_host_tail v0
  poke _p (C'clap_host_tail v0) = do
    pokeByteOff _p 0 v0
    return ()

{-# LINE 25 "src/Clap/Interface/Extension/Foreign/Tail.hsc" #-}
type C'clap_host_tail_t = C'clap_host_tail

{-# LINE 26 "src/Clap/Interface/Extension/Foreign/Tail.hsc" #-}
type C'changed = FunPtr (Ptr C'clap_host -> IO ())
foreign import ccall "wrapper" mk'changed
  :: (Ptr C'clap_host -> IO ()) -> IO C'changed
foreign import ccall "dynamic" mK'changed
  :: C'changed -> (Ptr C'clap_host -> IO ())
