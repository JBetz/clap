{-# LINE 1 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}


module Clap.Interface.Extension.Foreign.Params where
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

{-# LINE 7 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}

import Clap.Interface.Foreign.Events
import Clap.Interface.Foreign.Host
import Clap.Interface.Foreign.Plugin
-- #globalarray CLAP_EXT_PARAMS , CChar
{- enum {
    CLAP_PARAM_IS_STEPPED = 1 << 0,
    CLAP_PARAM_IS_PERIODIC = 1 << 1,
    CLAP_PARAM_IS_HIDDEN = 1 << 2,
    CLAP_PARAM_IS_READONLY = 1 << 3,
    CLAP_PARAM_IS_BYPASS = 1 << 4,
    CLAP_PARAM_IS_AUTOMATABLE = 1 << 5,
    CLAP_PARAM_IS_AUTOMATABLE_PER_NOTE_ID = 1 << 6,
    CLAP_PARAM_IS_AUTOMATABLE_PER_KEY = 1 << 7,
    CLAP_PARAM_IS_AUTOMATABLE_PER_CHANNEL = 1 << 8,
    CLAP_PARAM_IS_AUTOMATABLE_PER_PORT = 1 << 9,
    CLAP_PARAM_IS_MODULATABLE = 1 << 10,
    CLAP_PARAM_IS_MODULATABLE_PER_NOTE_ID = 1 << 11,
    CLAP_PARAM_IS_MODULATABLE_PER_KEY = 1 << 12,
    CLAP_PARAM_IS_MODULATABLE_PER_CHANNEL = 1 << 13,
    CLAP_PARAM_IS_MODULATABLE_PER_PORT = 1 << 14,
    CLAP_PARAM_REQUIRES_PROCESS = 1 << 15
}; -}
c'CLAP_PARAM_IS_STEPPED = 1
c'CLAP_PARAM_IS_STEPPED :: (Num a) => a

{-# LINE 31 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}
c'CLAP_PARAM_IS_PERIODIC = 2
c'CLAP_PARAM_IS_PERIODIC :: (Num a) => a

{-# LINE 32 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}
c'CLAP_PARAM_IS_HIDDEN = 4
c'CLAP_PARAM_IS_HIDDEN :: (Num a) => a

{-# LINE 33 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}
c'CLAP_PARAM_IS_READONLY = 8
c'CLAP_PARAM_IS_READONLY :: (Num a) => a

{-# LINE 34 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}
c'CLAP_PARAM_IS_BYPASS = 16
c'CLAP_PARAM_IS_BYPASS :: (Num a) => a

{-# LINE 35 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}
c'CLAP_PARAM_IS_AUTOMATABLE = 32
c'CLAP_PARAM_IS_AUTOMATABLE :: (Num a) => a

{-# LINE 36 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}
c'CLAP_PARAM_IS_AUTOMATABLE_PER_NOTE_ID = 64
c'CLAP_PARAM_IS_AUTOMATABLE_PER_NOTE_ID :: (Num a) => a

{-# LINE 37 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}
c'CLAP_PARAM_IS_AUTOMATABLE_PER_KEY = 128
c'CLAP_PARAM_IS_AUTOMATABLE_PER_KEY :: (Num a) => a

{-# LINE 38 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}
c'CLAP_PARAM_IS_AUTOMATABLE_PER_CHANNEL = 256
c'CLAP_PARAM_IS_AUTOMATABLE_PER_CHANNEL :: (Num a) => a

{-# LINE 39 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}
c'CLAP_PARAM_IS_AUTOMATABLE_PER_PORT = 512
c'CLAP_PARAM_IS_AUTOMATABLE_PER_PORT :: (Num a) => a

{-# LINE 40 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}
c'CLAP_PARAM_IS_MODULATABLE = 1024
c'CLAP_PARAM_IS_MODULATABLE :: (Num a) => a

{-# LINE 41 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}
c'CLAP_PARAM_IS_MODULATABLE_PER_NOTE_ID = 2048
c'CLAP_PARAM_IS_MODULATABLE_PER_NOTE_ID :: (Num a) => a

{-# LINE 42 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}
c'CLAP_PARAM_IS_MODULATABLE_PER_KEY = 4096
c'CLAP_PARAM_IS_MODULATABLE_PER_KEY :: (Num a) => a

{-# LINE 43 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}
c'CLAP_PARAM_IS_MODULATABLE_PER_CHANNEL = 8192
c'CLAP_PARAM_IS_MODULATABLE_PER_CHANNEL :: (Num a) => a

{-# LINE 44 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}
c'CLAP_PARAM_IS_MODULATABLE_PER_PORT = 16384
c'CLAP_PARAM_IS_MODULATABLE_PER_PORT :: (Num a) => a

{-# LINE 45 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}
c'CLAP_PARAM_REQUIRES_PROCESS = 32768
c'CLAP_PARAM_REQUIRES_PROCESS :: (Num a) => a

{-# LINE 46 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}
{- typedef uint32_t clap_param_info_flags; -}
type C'clap_param_info_flags = CUInt

{-# LINE 48 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}
{- typedef struct clap_param_info {
            clap_id id;
            clap_param_info_flags flags;
            void * cookie;
            char name[CLAP_NAME_SIZE];
            char module[CLAP_PATH_SIZE];
            double min_value;
            double max_value;
            double default_value;
        } clap_param_info_t; -}

{-# LINE 59 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}

{-# LINE 60 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}

{-# LINE 61 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}

{-# LINE 62 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}

{-# LINE 63 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}

{-# LINE 64 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}

{-# LINE 65 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}

{-# LINE 66 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}

{-# LINE 67 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}
data C'clap_param_info = C'clap_param_info{
  c'clap_param_info'id :: CUInt,
  c'clap_param_info'flags :: CUInt,
  c'clap_param_info'cookie :: Ptr (),
  c'clap_param_info'name :: [CChar],
  c'clap_param_info'module :: [CChar],
  c'clap_param_info'min_value :: CDouble,
  c'clap_param_info'max_value :: CDouble,
  c'clap_param_info'default_value :: CDouble
} deriving (Eq,Show)
p'clap_param_info'id p = plusPtr p 0
p'clap_param_info'id :: Ptr (C'clap_param_info) -> Ptr (CUInt)
p'clap_param_info'flags p = plusPtr p 4
p'clap_param_info'flags :: Ptr (C'clap_param_info) -> Ptr (CUInt)
p'clap_param_info'cookie p = plusPtr p 8
p'clap_param_info'cookie :: Ptr (C'clap_param_info) -> Ptr (Ptr ())
p'clap_param_info'name p = plusPtr p 16
p'clap_param_info'name :: Ptr (C'clap_param_info) -> Ptr (CChar)
p'clap_param_info'module p = plusPtr p 272
p'clap_param_info'module :: Ptr (C'clap_param_info) -> Ptr (CChar)
p'clap_param_info'min_value p = plusPtr p 1296
p'clap_param_info'min_value :: Ptr (C'clap_param_info) -> Ptr (CDouble)
p'clap_param_info'max_value p = plusPtr p 1304
p'clap_param_info'max_value :: Ptr (C'clap_param_info) -> Ptr (CDouble)
p'clap_param_info'default_value p = plusPtr p 1312
p'clap_param_info'default_value :: Ptr (C'clap_param_info) -> Ptr (CDouble)
instance Storable C'clap_param_info where
  sizeOf _ = 1320
  alignment _ = 8
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 4
    v2 <- peekByteOff _p 8
    v3 <- let s3 = div 256 $ sizeOf $ (undefined :: CChar) in peekArray s3 (plusPtr _p 16)
    v4 <- let s4 = div 1024 $ sizeOf $ (undefined :: CChar) in peekArray s4 (plusPtr _p 272)
    v5 <- peekByteOff _p 1296
    v6 <- peekByteOff _p 1304
    v7 <- peekByteOff _p 1312
    return $ C'clap_param_info v0 v1 v2 v3 v4 v5 v6 v7
  poke _p (C'clap_param_info v0 v1 v2 v3 v4 v5 v6 v7) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 4 v1
    pokeByteOff _p 8 v2
    let s3 = div 256 $ sizeOf $ (undefined :: CChar)
    pokeArray (plusPtr _p 16) (take s3 v3)
    let s4 = div 1024 $ sizeOf $ (undefined :: CChar)
    pokeArray (plusPtr _p 272) (take s4 v4)
    pokeByteOff _p 1296 v5
    pokeByteOff _p 1304 v6
    pokeByteOff _p 1312 v7
    return ()

{-# LINE 68 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}
type C'clap_param_info_t = C'clap_param_info

{-# LINE 69 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}
{- typedef struct clap_plugin_params {
            uint32_t (* count)(const clap_plugin_t * plugin);
            _Bool (* get_info)(const clap_plugin_t * plugin,
                               uint32_t param_index,
                               clap_param_info_t * param_info);
            _Bool (* get_value)(const clap_plugin_t * plugin,
                                clap_id param_id,
                                double * out_value);
            _Bool (* value_to_text)(const clap_plugin_t * plugin,
                                    clap_id param_id,
                                    double value,
                                    char * out_buffer,
                                    uint32_t out_buffer_capacity);
            _Bool (* text_to_value)(const clap_plugin_t * plugin,
                                    clap_id param_id,
                                    const char * param_value_text,
                                    double * out_value);
            void (* flush)(const clap_plugin_t * plugin,
                           const clap_input_events_t * in,
                           const clap_output_events_t * out);
        } clap_plugin_params_t; -}

{-# LINE 91 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}

{-# LINE 92 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}

{-# LINE 93 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}

{-# LINE 94 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}

{-# LINE 95 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}

{-# LINE 96 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}

{-# LINE 97 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}
data C'clap_plugin_params = C'clap_plugin_params{
  c'clap_plugin_params'count :: FunPtr (Ptr C'clap_plugin -> CUInt),
  c'clap_plugin_params'get_info :: FunPtr (Ptr C'clap_plugin -> CUInt -> Ptr C'clap_param_info -> CBool),
  c'clap_plugin_params'get_value :: FunPtr (Ptr C'clap_plugin -> CUInt -> Ptr CDouble -> CBool),
  c'clap_plugin_params'value_to_text :: FunPtr (Ptr C'clap_plugin -> CUInt -> CDouble -> CString -> CUInt -> CBool),
  c'clap_plugin_params'text_to_value :: FunPtr (Ptr C'clap_plugin -> CUInt -> CString -> Ptr CDouble -> CBool),
  c'clap_plugin_params'flush :: FunPtr (Ptr C'clap_plugin -> Ptr C'clap_input_events -> Ptr C'clap_output_events -> IO ())
} deriving (Eq,Show)
p'clap_plugin_params'count p = plusPtr p 0
p'clap_plugin_params'count :: Ptr (C'clap_plugin_params) -> Ptr (FunPtr (Ptr C'clap_plugin -> CUInt))
p'clap_plugin_params'get_info p = plusPtr p 8
p'clap_plugin_params'get_info :: Ptr (C'clap_plugin_params) -> Ptr (FunPtr (Ptr C'clap_plugin -> CUInt -> Ptr C'clap_param_info -> CBool))
p'clap_plugin_params'get_value p = plusPtr p 16
p'clap_plugin_params'get_value :: Ptr (C'clap_plugin_params) -> Ptr (FunPtr (Ptr C'clap_plugin -> CUInt -> Ptr CDouble -> CBool))
p'clap_plugin_params'value_to_text p = plusPtr p 24
p'clap_plugin_params'value_to_text :: Ptr (C'clap_plugin_params) -> Ptr (FunPtr (Ptr C'clap_plugin -> CUInt -> CDouble -> CString -> CUInt -> CBool))
p'clap_plugin_params'text_to_value p = plusPtr p 32
p'clap_plugin_params'text_to_value :: Ptr (C'clap_plugin_params) -> Ptr (FunPtr (Ptr C'clap_plugin -> CUInt -> CString -> Ptr CDouble -> CBool))
p'clap_plugin_params'flush p = plusPtr p 40
p'clap_plugin_params'flush :: Ptr (C'clap_plugin_params) -> Ptr (FunPtr (Ptr C'clap_plugin -> Ptr C'clap_input_events -> Ptr C'clap_output_events -> IO ()))
instance Storable C'clap_plugin_params where
  sizeOf _ = 48
  alignment _ = 8
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 8
    v2 <- peekByteOff _p 16
    v3 <- peekByteOff _p 24
    v4 <- peekByteOff _p 32
    v5 <- peekByteOff _p 40
    return $ C'clap_plugin_params v0 v1 v2 v3 v4 v5
  poke _p (C'clap_plugin_params v0 v1 v2 v3 v4 v5) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 8 v1
    pokeByteOff _p 16 v2
    pokeByteOff _p 24 v3
    pokeByteOff _p 32 v4
    pokeByteOff _p 40 v5
    return ()

{-# LINE 98 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}
type C'clap_plugin_params_t = C'clap_plugin_params

{-# LINE 99 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}
type C'count = FunPtr (Ptr C'clap_plugin -> CUInt)
foreign import ccall "wrapper" mk'count
  :: (Ptr C'clap_plugin -> CUInt) -> IO C'count
foreign import ccall "dynamic" mK'count
  :: C'count -> (Ptr C'clap_plugin -> CUInt)

{-# LINE 100 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}
type C'get_info = FunPtr (Ptr C'clap_plugin -> CUInt -> Ptr C'clap_param_info -> CBool)
foreign import ccall "wrapper" mk'get_info
  :: (Ptr C'clap_plugin -> CUInt -> Ptr C'clap_param_info -> CBool) -> IO C'get_info
foreign import ccall "dynamic" mK'get_info
  :: C'get_info -> (Ptr C'clap_plugin -> CUInt -> Ptr C'clap_param_info -> CBool)

{-# LINE 101 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}
type C'get_value = FunPtr (Ptr C'clap_plugin -> CUInt -> Ptr CDouble -> CBool)
foreign import ccall "wrapper" mk'get_value
  :: (Ptr C'clap_plugin -> CUInt -> Ptr CDouble -> CBool) -> IO C'get_value
foreign import ccall "dynamic" mK'get_value
  :: C'get_value -> (Ptr C'clap_plugin -> CUInt -> Ptr CDouble -> CBool)

{-# LINE 102 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}
type C'value_to_text = FunPtr (Ptr C'clap_plugin -> CUInt -> CDouble -> CString -> CUInt -> CBool)
foreign import ccall "wrapper" mk'value_to_text
  :: (Ptr C'clap_plugin -> CUInt -> CDouble -> CString -> CUInt -> CBool) -> IO C'value_to_text
foreign import ccall "dynamic" mK'value_to_text
  :: C'value_to_text -> (Ptr C'clap_plugin -> CUInt -> CDouble -> CString -> CUInt -> CBool)

{-# LINE 103 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}
type C'text_to_value = FunPtr (Ptr C'clap_plugin -> CUInt -> CString -> Ptr CDouble -> CBool)
foreign import ccall "wrapper" mk'text_to_value
  :: (Ptr C'clap_plugin -> CUInt -> CString -> Ptr CDouble -> CBool) -> IO C'text_to_value
foreign import ccall "dynamic" mK'text_to_value
  :: C'text_to_value -> (Ptr C'clap_plugin -> CUInt -> CString -> Ptr CDouble -> CBool)

{-# LINE 104 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}
type C'flush = FunPtr (Ptr C'clap_plugin -> Ptr C'clap_input_events -> Ptr C'clap_output_events -> IO ())
foreign import ccall "wrapper" mk'flush
  :: (Ptr C'clap_plugin -> Ptr C'clap_input_events -> Ptr C'clap_output_events -> IO ()) -> IO C'flush
foreign import ccall "dynamic" mK'flush
  :: C'flush -> (Ptr C'clap_plugin -> Ptr C'clap_input_events -> Ptr C'clap_output_events -> IO ())

{-# LINE 105 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}
{- enum {
    CLAP_PARAM_RESCAN_VALUES = 1 << 0,
    CLAP_PARAM_RESCAN_TEXT = 1 << 1,
    CLAP_PARAM_RESCAN_INFO = 1 << 2,
    CLAP_PARAM_RESCAN_ALL = 1 << 3
}; -}
c'CLAP_PARAM_RESCAN_VALUES = 1
c'CLAP_PARAM_RESCAN_VALUES :: (Num a) => a

{-# LINE 112 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}
c'CLAP_PARAM_RESCAN_TEXT = 2
c'CLAP_PARAM_RESCAN_TEXT :: (Num a) => a

{-# LINE 113 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}
c'CLAP_PARAM_RESCAN_INFO = 4
c'CLAP_PARAM_RESCAN_INFO :: (Num a) => a

{-# LINE 114 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}
c'CLAP_PARAM_RESCAN_ALL = 8
c'CLAP_PARAM_RESCAN_ALL :: (Num a) => a

{-# LINE 115 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}
{- typedef uint32_t clap_param_rescan_flags; -}
type C'clap_param_rescan_flags = CUInt

{-# LINE 117 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}
{- enum {
    CLAP_PARAM_CLEAR_ALL = 1 << 0,
    CLAP_PARAM_CLEAR_AUTOMATIONS = 1 << 1,
    CLAP_PARAM_CLEAR_MODULATIONS = 1 << 2
}; -}
c'CLAP_PARAM_CLEAR_ALL = 1
c'CLAP_PARAM_CLEAR_ALL :: (Num a) => a

{-# LINE 123 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}
c'CLAP_PARAM_CLEAR_AUTOMATIONS = 2
c'CLAP_PARAM_CLEAR_AUTOMATIONS :: (Num a) => a

{-# LINE 124 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}
c'CLAP_PARAM_CLEAR_MODULATIONS = 4
c'CLAP_PARAM_CLEAR_MODULATIONS :: (Num a) => a

{-# LINE 125 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}
{- typedef uint32_t clap_param_clear_flags; -}
type C'clap_param_clear_flags = CUInt

{-# LINE 127 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}
{- typedef struct clap_host_params {
            void (* rescan)(const clap_host_t * host,
                            clap_param_rescan_flags flags);
            void (* clear)(const clap_host_t * host,
                           clap_id param_id,
                           clap_param_clear_flags flags);
            void (* request_flush)(const clap_host_t * host);
        } clap_host_params_t; -}

{-# LINE 136 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}

{-# LINE 137 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}

{-# LINE 138 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}

{-# LINE 139 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}
data C'clap_host_params = C'clap_host_params{
  c'clap_host_params'rescan :: FunPtr (Ptr C'clap_host -> CUInt -> IO ()),
  c'clap_host_params'clear :: FunPtr (Ptr C'clap_host -> CUInt -> CUInt -> IO ()),
  c'clap_host_params'request_flush :: FunPtr (Ptr C'clap_host -> IO ())
} deriving (Eq,Show)
p'clap_host_params'rescan p = plusPtr p 0
p'clap_host_params'rescan :: Ptr (C'clap_host_params) -> Ptr (FunPtr (Ptr C'clap_host -> CUInt -> IO ()))
p'clap_host_params'clear p = plusPtr p 8
p'clap_host_params'clear :: Ptr (C'clap_host_params) -> Ptr (FunPtr (Ptr C'clap_host -> CUInt -> CUInt -> IO ()))
p'clap_host_params'request_flush p = plusPtr p 16
p'clap_host_params'request_flush :: Ptr (C'clap_host_params) -> Ptr (FunPtr (Ptr C'clap_host -> IO ()))
instance Storable C'clap_host_params where
  sizeOf _ = 24
  alignment _ = 8
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 8
    v2 <- peekByteOff _p 16
    return $ C'clap_host_params v0 v1 v2
  poke _p (C'clap_host_params v0 v1 v2) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 8 v1
    pokeByteOff _p 16 v2
    return ()

{-# LINE 140 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}
type C'clap_host_params_t = C'clap_host_params

{-# LINE 141 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}
type C'rescan = FunPtr (Ptr C'clap_host -> CUInt -> IO ())
foreign import ccall "wrapper" mk'rescan
  :: (Ptr C'clap_host -> CUInt -> IO ()) -> IO C'rescan
foreign import ccall "dynamic" mK'rescan
  :: C'rescan -> (Ptr C'clap_host -> CUInt -> IO ())

{-# LINE 142 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}
type C'clear = FunPtr (Ptr C'clap_host -> CUInt -> CUInt -> IO ())
foreign import ccall "wrapper" mk'clear
  :: (Ptr C'clap_host -> CUInt -> CUInt -> IO ()) -> IO C'clear
foreign import ccall "dynamic" mK'clear
  :: C'clear -> (Ptr C'clap_host -> CUInt -> CUInt -> IO ())

{-# LINE 143 "src/Clap/Interface/Extension/Foreign/Params.hsc" #-}
type C'request_flush = FunPtr (Ptr C'clap_host -> IO ())
foreign import ccall "wrapper" mk'request_flush
  :: (Ptr C'clap_host -> IO ()) -> IO C'request_flush
foreign import ccall "dynamic" mK'request_flush
  :: C'request_flush -> (Ptr C'clap_host -> IO ())
