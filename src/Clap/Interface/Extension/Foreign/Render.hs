{-# LINE 1 "src/Clap/Interface/Extension/Foreign/Render.hsc" #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}


module Clap.Interface.Extension.Foreign.Render where
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

{-# LINE 7 "src/Clap/Interface/Extension/Foreign/Render.hsc" #-}

import Clap.Interface.Foreign.Plugin
-- #globalarray CLAP_EXT_RENDER , CChar
{- enum {
    CLAP_RENDER_REALTIME = 0, CLAP_RENDER_OFFLINE = 1
}; -}
c'CLAP_RENDER_REALTIME = 0
c'CLAP_RENDER_REALTIME :: (Num a) => a

{-# LINE 14 "src/Clap/Interface/Extension/Foreign/Render.hsc" #-}
c'CLAP_RENDER_OFFLINE = 1
c'CLAP_RENDER_OFFLINE :: (Num a) => a

{-# LINE 15 "src/Clap/Interface/Extension/Foreign/Render.hsc" #-}
{- typedef int32_t clap_plugin_render_mode; -}
type C'clap_plugin_render_mode = CInt

{-# LINE 17 "src/Clap/Interface/Extension/Foreign/Render.hsc" #-}
{- typedef struct clap_plugin_render {
            _Bool (* has_hard_realtime_requirement)(const clap_plugin_t * plugin);
            _Bool (* set)(const clap_plugin_t * plugin,
                          clap_plugin_render_mode mode);
        } clap_plugin_render_t; -}

{-# LINE 23 "src/Clap/Interface/Extension/Foreign/Render.hsc" #-}

{-# LINE 24 "src/Clap/Interface/Extension/Foreign/Render.hsc" #-}

{-# LINE 25 "src/Clap/Interface/Extension/Foreign/Render.hsc" #-}
data C'clap_plugin_render = C'clap_plugin_render{
  c'clap_plugin_render'has_hard_realtime_requirement :: FunPtr (Ptr C'clap_plugin -> CBool),
  c'clap_plugin_render'set :: FunPtr (Ptr C'clap_plugin -> CInt -> CBool)
} deriving (Eq,Show)
p'clap_plugin_render'has_hard_realtime_requirement p = plusPtr p 0
p'clap_plugin_render'has_hard_realtime_requirement :: Ptr (C'clap_plugin_render) -> Ptr (FunPtr (Ptr C'clap_plugin -> CBool))
p'clap_plugin_render'set p = plusPtr p 8
p'clap_plugin_render'set :: Ptr (C'clap_plugin_render) -> Ptr (FunPtr (Ptr C'clap_plugin -> CInt -> CBool))
instance Storable C'clap_plugin_render where
  sizeOf _ = 16
  alignment _ = 8
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 8
    return $ C'clap_plugin_render v0 v1
  poke _p (C'clap_plugin_render v0 v1) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 8 v1
    return ()

{-# LINE 26 "src/Clap/Interface/Extension/Foreign/Render.hsc" #-}
type C'clap_plugin_render_t = C'clap_plugin_render

{-# LINE 27 "src/Clap/Interface/Extension/Foreign/Render.hsc" #-}
type C'has_hard_realtime_requirement = FunPtr (Ptr C'clap_plugin -> CBool)
foreign import ccall "wrapper" mk'has_hard_realtime_requirement
  :: (Ptr C'clap_plugin -> CBool) -> IO C'has_hard_realtime_requirement
foreign import ccall "dynamic" mK'has_hard_realtime_requirement
  :: C'has_hard_realtime_requirement -> (Ptr C'clap_plugin -> CBool)

{-# LINE 28 "src/Clap/Interface/Extension/Foreign/Render.hsc" #-}
type C'set = FunPtr (Ptr C'clap_plugin -> CInt -> CBool)
foreign import ccall "wrapper" mk'set
  :: (Ptr C'clap_plugin -> CInt -> CBool) -> IO C'set
foreign import ccall "dynamic" mK'set
  :: C'set -> (Ptr C'clap_plugin -> CInt -> CBool)
