{-# LINE 1 "src/Clap/Interface/Extension/Foreign/State.hsc" #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}


module Clap.Interface.Extension.Foreign.State where
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

{-# LINE 7 "src/Clap/Interface/Extension/Foreign/State.hsc" #-}

import Clap.Interface.Foreign.Host
import Clap.Interface.Foreign.Plugin
import Clap.Interface.Foreign.Stream
-- #globalarray CLAP_EXT_STATE , CChar
{- typedef struct clap_plugin_state {
            _Bool (* save)(const clap_plugin_t * plugin,
                           const clap_ostream_t * stream);
            _Bool (* load)(const clap_plugin_t * plugin,
                           const clap_istream_t * stream);
        } clap_plugin_state_t; -}

{-# LINE 19 "src/Clap/Interface/Extension/Foreign/State.hsc" #-}

{-# LINE 20 "src/Clap/Interface/Extension/Foreign/State.hsc" #-}

{-# LINE 21 "src/Clap/Interface/Extension/Foreign/State.hsc" #-}
data C'clap_plugin_state = C'clap_plugin_state{
  c'clap_plugin_state'save :: FunPtr (Ptr C'clap_plugin -> Ptr C'clap_ostream -> CBool),
  c'clap_plugin_state'load :: FunPtr (Ptr C'clap_plugin -> Ptr C'clap_istream -> CBool)
} deriving (Eq,Show)
p'clap_plugin_state'save p = plusPtr p 0
p'clap_plugin_state'save :: Ptr (C'clap_plugin_state) -> Ptr (FunPtr (Ptr C'clap_plugin -> Ptr C'clap_ostream -> CBool))
p'clap_plugin_state'load p = plusPtr p 8
p'clap_plugin_state'load :: Ptr (C'clap_plugin_state) -> Ptr (FunPtr (Ptr C'clap_plugin -> Ptr C'clap_istream -> CBool))
instance Storable C'clap_plugin_state where
  sizeOf _ = 16
  alignment _ = 8
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 8
    return $ C'clap_plugin_state v0 v1
  poke _p (C'clap_plugin_state v0 v1) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 8 v1
    return ()

{-# LINE 22 "src/Clap/Interface/Extension/Foreign/State.hsc" #-}
type C'clap_plugin_state_t = C'clap_plugin_state

{-# LINE 23 "src/Clap/Interface/Extension/Foreign/State.hsc" #-}
type C'save = FunPtr (Ptr C'clap_plugin -> Ptr C'clap_ostream -> CBool)
foreign import ccall "wrapper" mk'save
  :: (Ptr C'clap_plugin -> Ptr C'clap_ostream -> CBool) -> IO C'save
foreign import ccall "dynamic" mK'save
  :: C'save -> (Ptr C'clap_plugin -> Ptr C'clap_ostream -> CBool)

{-# LINE 24 "src/Clap/Interface/Extension/Foreign/State.hsc" #-}
type C'load = FunPtr (Ptr C'clap_plugin -> Ptr C'clap_istream -> CBool)
foreign import ccall "wrapper" mk'load
  :: (Ptr C'clap_plugin -> Ptr C'clap_istream -> CBool) -> IO C'load
foreign import ccall "dynamic" mK'load
  :: C'load -> (Ptr C'clap_plugin -> Ptr C'clap_istream -> CBool)

{-# LINE 25 "src/Clap/Interface/Extension/Foreign/State.hsc" #-}
{- typedef struct clap_host_state {
            void (* mark_dirty)(const clap_host_t * host);
        } clap_host_state_t; -}

{-# LINE 29 "src/Clap/Interface/Extension/Foreign/State.hsc" #-}

{-# LINE 30 "src/Clap/Interface/Extension/Foreign/State.hsc" #-}
data C'clap_host_state = C'clap_host_state{
  c'clap_host_state'mark_dirty :: FunPtr (Ptr C'clap_host -> IO ())
} deriving (Eq,Show)
p'clap_host_state'mark_dirty p = plusPtr p 0
p'clap_host_state'mark_dirty :: Ptr (C'clap_host_state) -> Ptr (FunPtr (Ptr C'clap_host -> IO ()))
instance Storable C'clap_host_state where
  sizeOf _ = 8
  alignment _ = 8
  peek _p = do
    v0 <- peekByteOff _p 0
    return $ C'clap_host_state v0
  poke _p (C'clap_host_state v0) = do
    pokeByteOff _p 0 v0
    return ()

{-# LINE 31 "src/Clap/Interface/Extension/Foreign/State.hsc" #-}
type C'clap_host_state_t = C'clap_host_state

{-# LINE 32 "src/Clap/Interface/Extension/Foreign/State.hsc" #-}
type C'mark_dirty = FunPtr (Ptr C'clap_host -> IO ())
foreign import ccall "wrapper" mk'mark_dirty
  :: (Ptr C'clap_host -> IO ()) -> IO C'mark_dirty
foreign import ccall "dynamic" mK'mark_dirty
  :: C'mark_dirty -> (Ptr C'clap_host -> IO ())

{-# LINE 33 "src/Clap/Interface/Extension/Foreign/State.hsc" #-}
