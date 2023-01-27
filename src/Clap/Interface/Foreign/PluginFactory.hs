{-# LINE 1 "src/Clap/Interface/Foreign/PluginFactory.hsc" #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}


module Clap.Interface.Foreign.PluginFactory where
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

{-# LINE 7 "src/Clap/Interface/Foreign/PluginFactory.hsc" #-}

import Clap.Interface.Foreign.Host
import Clap.Interface.Foreign.Plugin
{- typedef struct clap_plugin_factory {
            uint32_t (* get_plugin_count)(const struct clap_plugin_factory * factory);
            const clap_plugin_descriptor_t * (* get_plugin_descriptor)(const struct clap_plugin_factory * factory,
                                                                       uint32_t index);
            const clap_plugin_t * (* create_plugin)(const struct clap_plugin_factory * factory,
                                                    const clap_host_t * host,
                                                    const char * plugin_id);
        } clap_plugin_factory_t; -}

{-# LINE 19 "src/Clap/Interface/Foreign/PluginFactory.hsc" #-}

{-# LINE 20 "src/Clap/Interface/Foreign/PluginFactory.hsc" #-}

{-# LINE 21 "src/Clap/Interface/Foreign/PluginFactory.hsc" #-}

{-# LINE 22 "src/Clap/Interface/Foreign/PluginFactory.hsc" #-}
data C'clap_plugin_factory = C'clap_plugin_factory{
  c'clap_plugin_factory'get_plugin_count :: FunPtr (Ptr C'clap_plugin_factory -> CUInt),
  c'clap_plugin_factory'get_plugin_descriptor :: FunPtr (Ptr C'clap_plugin_factory -> CUInt -> Ptr C'clap_plugin_descriptor),
  c'clap_plugin_factory'create_plugin :: FunPtr (Ptr C'clap_plugin_factory -> Ptr C'clap_host -> CString -> Ptr C'clap_plugin)
} deriving (Eq,Show)
p'clap_plugin_factory'get_plugin_count p = plusPtr p 0
p'clap_plugin_factory'get_plugin_count :: Ptr (C'clap_plugin_factory) -> Ptr (FunPtr (Ptr C'clap_plugin_factory -> CUInt))
p'clap_plugin_factory'get_plugin_descriptor p = plusPtr p 8
p'clap_plugin_factory'get_plugin_descriptor :: Ptr (C'clap_plugin_factory) -> Ptr (FunPtr (Ptr C'clap_plugin_factory -> CUInt -> Ptr C'clap_plugin_descriptor))
p'clap_plugin_factory'create_plugin p = plusPtr p 16
p'clap_plugin_factory'create_plugin :: Ptr (C'clap_plugin_factory) -> Ptr (FunPtr (Ptr C'clap_plugin_factory -> Ptr C'clap_host -> CString -> Ptr C'clap_plugin))
instance Storable C'clap_plugin_factory where
  sizeOf _ = 24
  alignment _ = 8
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 8
    v2 <- peekByteOff _p 16
    return $ C'clap_plugin_factory v0 v1 v2
  poke _p (C'clap_plugin_factory v0 v1 v2) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 8 v1
    pokeByteOff _p 16 v2
    return ()

{-# LINE 23 "src/Clap/Interface/Foreign/PluginFactory.hsc" #-}
type C'clap_plugin_factory_t = C'clap_plugin_factory

{-# LINE 24 "src/Clap/Interface/Foreign/PluginFactory.hsc" #-}
type C'get_plugin_count = FunPtr (Ptr C'clap_plugin_factory -> CUInt)
foreign import ccall "wrapper" mk'get_plugin_count
  :: (Ptr C'clap_plugin_factory -> CUInt) -> IO C'get_plugin_count
foreign import ccall "dynamic" mK'get_plugin_count
  :: C'get_plugin_count -> (Ptr C'clap_plugin_factory -> CUInt)

{-# LINE 25 "src/Clap/Interface/Foreign/PluginFactory.hsc" #-}
type C'get_plugin_descriptor = FunPtr (Ptr C'clap_plugin_factory -> CUInt -> Ptr C'clap_plugin_descriptor)
foreign import ccall "wrapper" mk'get_plugin_descriptor
  :: (Ptr C'clap_plugin_factory -> CUInt -> Ptr C'clap_plugin_descriptor) -> IO C'get_plugin_descriptor
foreign import ccall "dynamic" mK'get_plugin_descriptor
  :: C'get_plugin_descriptor -> (Ptr C'clap_plugin_factory -> CUInt -> Ptr C'clap_plugin_descriptor)

{-# LINE 26 "src/Clap/Interface/Foreign/PluginFactory.hsc" #-}
type C'create_plugin = FunPtr (Ptr C'clap_plugin_factory -> Ptr C'clap_host -> CString -> Ptr C'clap_plugin)
foreign import ccall "wrapper" mk'create_plugin
  :: (Ptr C'clap_plugin_factory -> Ptr C'clap_host -> CString -> Ptr C'clap_plugin) -> IO C'create_plugin
foreign import ccall "dynamic" mK'create_plugin
  :: C'create_plugin -> (Ptr C'clap_plugin_factory -> Ptr C'clap_host -> CString -> Ptr C'clap_plugin)
