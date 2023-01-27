{-# LINE 1 "src/Clap/Interface/Foreign/Plugin.hsc" #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}


module Clap.Interface.Foreign.Plugin where
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

{-# LINE 7 "src/Clap/Interface/Foreign/Plugin.hsc" #-}

import Clap.Interface.Foreign.Host (C'clap_host)
import Clap.Interface.Foreign.Process (C'clap_process, C'clap_process_status)
import Clap.Interface.Foreign.Version
{- typedef struct clap_plugin_descriptor {
            clap_version_t clap_version;
            const char * id;
            const char * name;
            const char * vendor;
            const char * url;
            const char * manual_url;
            const char * support_url;
            const char * version;
            const char * description;
            const char * const * features;
        } clap_plugin_descriptor_t; -}

{-# LINE 24 "src/Clap/Interface/Foreign/Plugin.hsc" #-}

{-# LINE 25 "src/Clap/Interface/Foreign/Plugin.hsc" #-}

{-# LINE 26 "src/Clap/Interface/Foreign/Plugin.hsc" #-}

{-# LINE 27 "src/Clap/Interface/Foreign/Plugin.hsc" #-}

{-# LINE 28 "src/Clap/Interface/Foreign/Plugin.hsc" #-}

{-# LINE 29 "src/Clap/Interface/Foreign/Plugin.hsc" #-}

{-# LINE 30 "src/Clap/Interface/Foreign/Plugin.hsc" #-}

{-# LINE 31 "src/Clap/Interface/Foreign/Plugin.hsc" #-}

{-# LINE 32 "src/Clap/Interface/Foreign/Plugin.hsc" #-}

{-# LINE 33 "src/Clap/Interface/Foreign/Plugin.hsc" #-}

{-# LINE 34 "src/Clap/Interface/Foreign/Plugin.hsc" #-}
data C'clap_plugin_descriptor = C'clap_plugin_descriptor{
  c'clap_plugin_descriptor'clap_version :: C'clap_version,
  c'clap_plugin_descriptor'id :: CString,
  c'clap_plugin_descriptor'name :: CString,
  c'clap_plugin_descriptor'vendor :: CString,
  c'clap_plugin_descriptor'url :: CString,
  c'clap_plugin_descriptor'manual_url :: CString,
  c'clap_plugin_descriptor'support_url :: CString,
  c'clap_plugin_descriptor'version :: CString,
  c'clap_plugin_descriptor'description :: CString,
  c'clap_plugin_descriptor'features :: Ptr CString
} deriving (Eq,Show)
p'clap_plugin_descriptor'clap_version p = plusPtr p 0
p'clap_plugin_descriptor'clap_version :: Ptr (C'clap_plugin_descriptor) -> Ptr (C'clap_version)
p'clap_plugin_descriptor'id p = plusPtr p 16
p'clap_plugin_descriptor'id :: Ptr (C'clap_plugin_descriptor) -> Ptr (CString)
p'clap_plugin_descriptor'name p = plusPtr p 24
p'clap_plugin_descriptor'name :: Ptr (C'clap_plugin_descriptor) -> Ptr (CString)
p'clap_plugin_descriptor'vendor p = plusPtr p 32
p'clap_plugin_descriptor'vendor :: Ptr (C'clap_plugin_descriptor) -> Ptr (CString)
p'clap_plugin_descriptor'url p = plusPtr p 40
p'clap_plugin_descriptor'url :: Ptr (C'clap_plugin_descriptor) -> Ptr (CString)
p'clap_plugin_descriptor'manual_url p = plusPtr p 48
p'clap_plugin_descriptor'manual_url :: Ptr (C'clap_plugin_descriptor) -> Ptr (CString)
p'clap_plugin_descriptor'support_url p = plusPtr p 56
p'clap_plugin_descriptor'support_url :: Ptr (C'clap_plugin_descriptor) -> Ptr (CString)
p'clap_plugin_descriptor'version p = plusPtr p 64
p'clap_plugin_descriptor'version :: Ptr (C'clap_plugin_descriptor) -> Ptr (CString)
p'clap_plugin_descriptor'description p = plusPtr p 72
p'clap_plugin_descriptor'description :: Ptr (C'clap_plugin_descriptor) -> Ptr (CString)
p'clap_plugin_descriptor'features p = plusPtr p 80
p'clap_plugin_descriptor'features :: Ptr (C'clap_plugin_descriptor) -> Ptr (Ptr CString)
instance Storable C'clap_plugin_descriptor where
  sizeOf _ = 88
  alignment _ = 8
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 16
    v2 <- peekByteOff _p 24
    v3 <- peekByteOff _p 32
    v4 <- peekByteOff _p 40
    v5 <- peekByteOff _p 48
    v6 <- peekByteOff _p 56
    v7 <- peekByteOff _p 64
    v8 <- peekByteOff _p 72
    v9 <- peekByteOff _p 80
    return $ C'clap_plugin_descriptor v0 v1 v2 v3 v4 v5 v6 v7 v8 v9
  poke _p (C'clap_plugin_descriptor v0 v1 v2 v3 v4 v5 v6 v7 v8 v9) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 16 v1
    pokeByteOff _p 24 v2
    pokeByteOff _p 32 v3
    pokeByteOff _p 40 v4
    pokeByteOff _p 48 v5
    pokeByteOff _p 56 v6
    pokeByteOff _p 64 v7
    pokeByteOff _p 72 v8
    pokeByteOff _p 80 v9
    return ()

{-# LINE 35 "src/Clap/Interface/Foreign/Plugin.hsc" #-}
type C'clap_plugin_descriptor_t = C'clap_plugin_descriptor

{-# LINE 36 "src/Clap/Interface/Foreign/Plugin.hsc" #-}
{- typedef struct clap_plugin {
            const clap_plugin_descriptor_t * desc;
            void * plugin_data;
            _Bool (* init)(const struct clap_plugin * plugin);
            void (* destroy)(const struct clap_plugin * plugin);
            _Bool (* activate)(const struct clap_plugin * plugin,
                               double sample_rate,
                               uint32_t min_frames_count,
                               uint32_t max_frames_count);
            void (* deactivate)(const struct clap_plugin * plugin);
            _Bool (* start_processing)(const struct clap_plugin * plugin);
            void (* stop_processing)(const struct clap_plugin * plugin);
            void (* reset)(const struct clap_plugin * plugin);
            clap_process_status (* process)(const struct clap_plugin * plugin,
                                            const clap_process_t * process);
            const void * (* get_extension)(const struct clap_plugin * plugin,
                                           const char * id);
            void (* on_main_thread)(const struct clap_plugin * plugin);
        } clap_plugin_t; -}

{-# LINE 56 "src/Clap/Interface/Foreign/Plugin.hsc" #-}

{-# LINE 57 "src/Clap/Interface/Foreign/Plugin.hsc" #-}

{-# LINE 58 "src/Clap/Interface/Foreign/Plugin.hsc" #-}

{-# LINE 59 "src/Clap/Interface/Foreign/Plugin.hsc" #-}

{-# LINE 60 "src/Clap/Interface/Foreign/Plugin.hsc" #-}

{-# LINE 61 "src/Clap/Interface/Foreign/Plugin.hsc" #-}

{-# LINE 62 "src/Clap/Interface/Foreign/Plugin.hsc" #-}

{-# LINE 63 "src/Clap/Interface/Foreign/Plugin.hsc" #-}

{-# LINE 64 "src/Clap/Interface/Foreign/Plugin.hsc" #-}

{-# LINE 65 "src/Clap/Interface/Foreign/Plugin.hsc" #-}

{-# LINE 66 "src/Clap/Interface/Foreign/Plugin.hsc" #-}

{-# LINE 67 "src/Clap/Interface/Foreign/Plugin.hsc" #-}

{-# LINE 68 "src/Clap/Interface/Foreign/Plugin.hsc" #-}
data C'clap_plugin = C'clap_plugin{
  c'clap_plugin'desc :: Ptr C'clap_plugin_descriptor,
  c'clap_plugin'plugin_data :: Ptr (),
  c'clap_plugin'init :: FunPtr (Ptr C'clap_plugin -> CBool),
  c'clap_plugin'destroy :: FunPtr (Ptr C'clap_plugin -> IO ()),
  c'clap_plugin'activate :: FunPtr (Ptr C'clap_plugin -> CDouble -> CUInt -> CUInt -> CBool),
  c'clap_plugin'deactivate :: FunPtr (Ptr C'clap_plugin -> IO ()),
  c'clap_plugin'start_processing :: FunPtr (Ptr C'clap_plugin -> CBool),
  c'clap_plugin'stop_processing :: FunPtr (Ptr C'clap_plugin -> IO ()),
  c'clap_plugin'reset :: FunPtr (Ptr C'clap_plugin -> IO ()),
  c'clap_plugin'process :: FunPtr (Ptr C'clap_plugin -> Ptr C'clap_process -> C'clap_process_status),
  c'clap_plugin'get_extension :: FunPtr (Ptr C'clap_plugin -> CString -> Ptr ()),
  c'clap_plugin'on_main_thread :: FunPtr (Ptr C'clap_plugin -> IO ())
} deriving (Eq,Show)
p'clap_plugin'desc p = plusPtr p 0
p'clap_plugin'desc :: Ptr (C'clap_plugin) -> Ptr (Ptr C'clap_plugin_descriptor)
p'clap_plugin'plugin_data p = plusPtr p 8
p'clap_plugin'plugin_data :: Ptr (C'clap_plugin) -> Ptr (Ptr ())
p'clap_plugin'init p = plusPtr p 16
p'clap_plugin'init :: Ptr (C'clap_plugin) -> Ptr (FunPtr (Ptr C'clap_plugin -> CBool))
p'clap_plugin'destroy p = plusPtr p 24
p'clap_plugin'destroy :: Ptr (C'clap_plugin) -> Ptr (FunPtr (Ptr C'clap_plugin -> IO ()))
p'clap_plugin'activate p = plusPtr p 32
p'clap_plugin'activate :: Ptr (C'clap_plugin) -> Ptr (FunPtr (Ptr C'clap_plugin -> CDouble -> CUInt -> CUInt -> CBool))
p'clap_plugin'deactivate p = plusPtr p 40
p'clap_plugin'deactivate :: Ptr (C'clap_plugin) -> Ptr (FunPtr (Ptr C'clap_plugin -> IO ()))
p'clap_plugin'start_processing p = plusPtr p 48
p'clap_plugin'start_processing :: Ptr (C'clap_plugin) -> Ptr (FunPtr (Ptr C'clap_plugin -> CBool))
p'clap_plugin'stop_processing p = plusPtr p 56
p'clap_plugin'stop_processing :: Ptr (C'clap_plugin) -> Ptr (FunPtr (Ptr C'clap_plugin -> IO ()))
p'clap_plugin'reset p = plusPtr p 64
p'clap_plugin'reset :: Ptr (C'clap_plugin) -> Ptr (FunPtr (Ptr C'clap_plugin -> IO ()))
p'clap_plugin'process p = plusPtr p 72
p'clap_plugin'process :: Ptr (C'clap_plugin) -> Ptr (FunPtr (Ptr C'clap_plugin -> Ptr C'clap_process -> C'clap_process_status))
p'clap_plugin'get_extension p = plusPtr p 80
p'clap_plugin'get_extension :: Ptr (C'clap_plugin) -> Ptr (FunPtr (Ptr C'clap_plugin -> CString -> Ptr ()))
p'clap_plugin'on_main_thread p = plusPtr p 88
p'clap_plugin'on_main_thread :: Ptr (C'clap_plugin) -> Ptr (FunPtr (Ptr C'clap_plugin -> IO ()))
instance Storable C'clap_plugin where
  sizeOf _ = 96
  alignment _ = 8
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 8
    v2 <- peekByteOff _p 16
    v3 <- peekByteOff _p 24
    v4 <- peekByteOff _p 32
    v5 <- peekByteOff _p 40
    v6 <- peekByteOff _p 48
    v7 <- peekByteOff _p 56
    v8 <- peekByteOff _p 64
    v9 <- peekByteOff _p 72
    v10 <- peekByteOff _p 80
    v11 <- peekByteOff _p 88
    return $ C'clap_plugin v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11
  poke _p (C'clap_plugin v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 8 v1
    pokeByteOff _p 16 v2
    pokeByteOff _p 24 v3
    pokeByteOff _p 32 v4
    pokeByteOff _p 40 v5
    pokeByteOff _p 48 v6
    pokeByteOff _p 56 v7
    pokeByteOff _p 64 v8
    pokeByteOff _p 72 v9
    pokeByteOff _p 80 v10
    pokeByteOff _p 88 v11
    return ()

{-# LINE 69 "src/Clap/Interface/Foreign/Plugin.hsc" #-}
type C'clap_plugin_t = C'clap_plugin

{-# LINE 70 "src/Clap/Interface/Foreign/Plugin.hsc" #-}
type C'init = FunPtr (Ptr C'clap_plugin -> CBool)
foreign import ccall "wrapper" mk'init
  :: (Ptr C'clap_plugin -> CBool) -> IO C'init
foreign import ccall "dynamic" mK'init
  :: C'init -> (Ptr C'clap_plugin -> CBool)

{-# LINE 71 "src/Clap/Interface/Foreign/Plugin.hsc" #-}
type C'destroy = FunPtr (Ptr C'clap_plugin -> IO ())
foreign import ccall "wrapper" mk'destroy
  :: (Ptr C'clap_plugin -> IO ()) -> IO C'destroy
foreign import ccall "dynamic" mK'destroy
  :: C'destroy -> (Ptr C'clap_plugin -> IO ())

{-# LINE 72 "src/Clap/Interface/Foreign/Plugin.hsc" #-}
type C'activate = FunPtr (Ptr C'clap_plugin -> CDouble -> CUInt -> CUInt -> CBool)
foreign import ccall "wrapper" mk'activate
  :: (Ptr C'clap_plugin -> CDouble -> CUInt -> CUInt -> CBool) -> IO C'activate
foreign import ccall "dynamic" mK'activate
  :: C'activate -> (Ptr C'clap_plugin -> CDouble -> CUInt -> CUInt -> CBool)

{-# LINE 73 "src/Clap/Interface/Foreign/Plugin.hsc" #-}
type C'deactivate = FunPtr (Ptr C'clap_plugin -> IO ())
foreign import ccall "wrapper" mk'deactivate
  :: (Ptr C'clap_plugin -> IO ()) -> IO C'deactivate
foreign import ccall "dynamic" mK'deactivate
  :: C'deactivate -> (Ptr C'clap_plugin -> IO ())

{-# LINE 74 "src/Clap/Interface/Foreign/Plugin.hsc" #-}
type C'start_processing = FunPtr (Ptr C'clap_plugin -> CBool)
foreign import ccall "wrapper" mk'start_processing
  :: (Ptr C'clap_plugin -> CBool) -> IO C'start_processing
foreign import ccall "dynamic" mK'start_processing
  :: C'start_processing -> (Ptr C'clap_plugin -> CBool)

{-# LINE 75 "src/Clap/Interface/Foreign/Plugin.hsc" #-}
type C'stop_processing = FunPtr (Ptr C'clap_plugin -> IO ())
foreign import ccall "wrapper" mk'stop_processing
  :: (Ptr C'clap_plugin -> IO ()) -> IO C'stop_processing
foreign import ccall "dynamic" mK'stop_processing
  :: C'stop_processing -> (Ptr C'clap_plugin -> IO ())

{-# LINE 76 "src/Clap/Interface/Foreign/Plugin.hsc" #-}
type C'reset = FunPtr (Ptr C'clap_plugin -> IO ())
foreign import ccall "wrapper" mk'reset
  :: (Ptr C'clap_plugin -> IO ()) -> IO C'reset
foreign import ccall "dynamic" mK'reset
  :: C'reset -> (Ptr C'clap_plugin -> IO ())

{-# LINE 77 "src/Clap/Interface/Foreign/Plugin.hsc" #-}
type C'process = FunPtr (Ptr C'clap_plugin -> Ptr C'clap_process -> C'clap_process_status)
foreign import ccall "wrapper" mk'process
  :: (Ptr C'clap_plugin -> Ptr C'clap_process -> C'clap_process_status) -> IO C'process
foreign import ccall "dynamic" mK'process
  :: C'process -> (Ptr C'clap_plugin -> Ptr C'clap_process -> C'clap_process_status)

{-# LINE 78 "src/Clap/Interface/Foreign/Plugin.hsc" #-}
type C'get_extension = FunPtr (Ptr C'clap_plugin -> CString -> Ptr ())
foreign import ccall "wrapper" mk'get_extension
  :: (Ptr C'clap_plugin -> CString -> Ptr ()) -> IO C'get_extension
foreign import ccall "dynamic" mK'get_extension
  :: C'get_extension -> (Ptr C'clap_plugin -> CString -> Ptr ())

{-# LINE 79 "src/Clap/Interface/Foreign/Plugin.hsc" #-}
type C'on_main_thread = FunPtr (Ptr C'clap_plugin -> IO ())
foreign import ccall "wrapper" mk'on_main_thread
  :: (Ptr C'clap_plugin -> IO ()) -> IO C'on_main_thread
foreign import ccall "dynamic" mK'on_main_thread
  :: C'on_main_thread -> (Ptr C'clap_plugin -> IO ())

{-# LINE 80 "src/Clap/Interface/Foreign/Plugin.hsc" #-}
