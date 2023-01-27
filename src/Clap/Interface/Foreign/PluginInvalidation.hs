{-# LINE 1 "src/Clap/Interface/Foreign/PluginInvalidation.hsc" #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}



module Clap.Interface.Foreign.PluginInvalidation where
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

{-# LINE 8 "src/Clap/Interface/Foreign/PluginInvalidation.hsc" #-}

{- typedef struct clap_plugin_invalidation_source {
            const char * directory;
            const char * filename_glob;
            _Bool recursive_scan;
        } clap_plugin_invalidation_source_t; -}

{-# LINE 15 "src/Clap/Interface/Foreign/PluginInvalidation.hsc" #-}

{-# LINE 16 "src/Clap/Interface/Foreign/PluginInvalidation.hsc" #-}

{-# LINE 17 "src/Clap/Interface/Foreign/PluginInvalidation.hsc" #-}

{-# LINE 18 "src/Clap/Interface/Foreign/PluginInvalidation.hsc" #-}
data C'clap_plugin_invalidation_source = C'clap_plugin_invalidation_source{
  c'clap_plugin_invalidation_source'directory :: CString,
  c'clap_plugin_invalidation_source'filename_glob :: CString,
  c'clap_plugin_invalidation_source'recursive_scan :: CInt
} deriving (Eq,Show)
p'clap_plugin_invalidation_source'directory p = plusPtr p 0
p'clap_plugin_invalidation_source'directory :: Ptr (C'clap_plugin_invalidation_source) -> Ptr (CString)
p'clap_plugin_invalidation_source'filename_glob p = plusPtr p 8
p'clap_plugin_invalidation_source'filename_glob :: Ptr (C'clap_plugin_invalidation_source) -> Ptr (CString)
p'clap_plugin_invalidation_source'recursive_scan p = plusPtr p 16
p'clap_plugin_invalidation_source'recursive_scan :: Ptr (C'clap_plugin_invalidation_source) -> Ptr (CInt)
instance Storable C'clap_plugin_invalidation_source where
  sizeOf _ = 24
  alignment _ = 8
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 8
    v2 <- peekByteOff _p 16
    return $ C'clap_plugin_invalidation_source v0 v1 v2
  poke _p (C'clap_plugin_invalidation_source v0 v1 v2) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 8 v1
    pokeByteOff _p 16 v2
    return ()

{-# LINE 19 "src/Clap/Interface/Foreign/PluginInvalidation.hsc" #-}
type C'clap_plugin_invalidation_source_t = C'clap_plugin_invalidation_source

{-# LINE 20 "src/Clap/Interface/Foreign/PluginInvalidation.hsc" #-}
foreign import ccall "array_CLAP_PLUGIN_INVALIDATION_FACTORY_ID" c'CLAP_PLUGIN_INVALIDATION_FACTORY_ID
  :: Ptr (CChar)

{-# LINE 21 "src/Clap/Interface/Foreign/PluginInvalidation.hsc" #-}
{- typedef struct clap_plugin_invalidation_factory {
            uint32_t (* count)(const struct clap_plugin_invalidation_factory * factory);
            const clap_plugin_invalidation_source_t * (* get)(const struct clap_plugin_invalidation_factory * factory,
                                                              uint32_t index);
            _Bool (* refresh)(const struct clap_plugin_invalidation_factory * factory);
        } clap_plugin_invalidation_factory_t; -}

{-# LINE 28 "src/Clap/Interface/Foreign/PluginInvalidation.hsc" #-}

{-# LINE 29 "src/Clap/Interface/Foreign/PluginInvalidation.hsc" #-}

{-# LINE 30 "src/Clap/Interface/Foreign/PluginInvalidation.hsc" #-}

{-# LINE 31 "src/Clap/Interface/Foreign/PluginInvalidation.hsc" #-}
data C'clap_plugin_invalidation_factory = C'clap_plugin_invalidation_factory{
  c'clap_plugin_invalidation_factory'count :: FunPtr (Ptr C'clap_plugin_invalidation_factory -> CUInt),
  c'clap_plugin_invalidation_factory'get :: FunPtr (Ptr C'clap_plugin_invalidation_factory -> CUInt -> Ptr C'clap_plugin_invalidation_source),
  c'clap_plugin_invalidation_factory'refresh :: FunPtr (Ptr C'clap_plugin_invalidation_factory -> CInt)
} deriving (Eq,Show)
p'clap_plugin_invalidation_factory'count p = plusPtr p 0
p'clap_plugin_invalidation_factory'count :: Ptr (C'clap_plugin_invalidation_factory) -> Ptr (FunPtr (Ptr C'clap_plugin_invalidation_factory -> CUInt))
p'clap_plugin_invalidation_factory'get p = plusPtr p 8
p'clap_plugin_invalidation_factory'get :: Ptr (C'clap_plugin_invalidation_factory) -> Ptr (FunPtr (Ptr C'clap_plugin_invalidation_factory -> CUInt -> Ptr C'clap_plugin_invalidation_source))
p'clap_plugin_invalidation_factory'refresh p = plusPtr p 16
p'clap_plugin_invalidation_factory'refresh :: Ptr (C'clap_plugin_invalidation_factory) -> Ptr (FunPtr (Ptr C'clap_plugin_invalidation_factory -> CInt))
instance Storable C'clap_plugin_invalidation_factory where
  sizeOf _ = 24
  alignment _ = 8
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 8
    v2 <- peekByteOff _p 16
    return $ C'clap_plugin_invalidation_factory v0 v1 v2
  poke _p (C'clap_plugin_invalidation_factory v0 v1 v2) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 8 v1
    pokeByteOff _p 16 v2
    return ()

{-# LINE 32 "src/Clap/Interface/Foreign/PluginInvalidation.hsc" #-}
type C'clap_plugin_invalidation_factory_t = C'clap_plugin_invalidation_factory

{-# LINE 33 "src/Clap/Interface/Foreign/PluginInvalidation.hsc" #-}
type C'count = FunPtr (Ptr C'clap_plugin_invalidation_factory -> CUInt)
foreign import ccall "wrapper" mk'count
  :: (Ptr C'clap_plugin_invalidation_factory -> CUInt) -> IO C'count
foreign import ccall "dynamic" mK'count
  :: C'count -> (Ptr C'clap_plugin_invalidation_factory -> CUInt)

{-# LINE 34 "src/Clap/Interface/Foreign/PluginInvalidation.hsc" #-}
type C'get = FunPtr (Ptr C'clap_plugin_invalidation_factory -> CUInt -> Ptr C'clap_plugin_invalidation_source)
foreign import ccall "wrapper" mk'get
  :: (Ptr C'clap_plugin_invalidation_factory -> CUInt -> Ptr C'clap_plugin_invalidation_source) -> IO C'get
foreign import ccall "dynamic" mK'get
  :: C'get -> (Ptr C'clap_plugin_invalidation_factory -> CUInt -> Ptr C'clap_plugin_invalidation_source)

{-# LINE 35 "src/Clap/Interface/Foreign/PluginInvalidation.hsc" #-}
type C'refresh = FunPtr (Ptr C'clap_plugin_invalidation_factory -> CInt)
foreign import ccall "wrapper" mk'refresh
  :: (Ptr C'clap_plugin_invalidation_factory -> CInt) -> IO C'refresh
foreign import ccall "dynamic" mK'refresh
  :: C'refresh -> (Ptr C'clap_plugin_invalidation_factory -> CInt)

{-# LINE 36 "src/Clap/Interface/Foreign/PluginInvalidation.hsc" #-}
