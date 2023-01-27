{-# LINE 1 "src/Clap/Interface/Extension/Foreign/AudioPorts.hsc" #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}



module Clap.Interface.Extension.Foreign.AudioPorts where
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

{-# LINE 8 "src/Clap/Interface/Extension/Foreign/AudioPorts.hsc" #-}

import Clap.Interface.Foreign.Host
import Clap.Interface.Foreign.Plugin
foreign import ccall "array_CLAP_EXT_AUDIO_PORTS" c'CLAP_EXT_AUDIO_PORTS
  :: Ptr (CChar)

{-# LINE 12 "src/Clap/Interface/Extension/Foreign/AudioPorts.hsc" #-}
foreign import ccall "array_CLAP_PORT_MONO" c'CLAP_PORT_MONO
  :: Ptr (CChar)

{-# LINE 13 "src/Clap/Interface/Extension/Foreign/AudioPorts.hsc" #-}
foreign import ccall "array_CLAP_PORT_STEREO" c'CLAP_PORT_STEREO
  :: Ptr (CChar)

{-# LINE 14 "src/Clap/Interface/Extension/Foreign/AudioPorts.hsc" #-}
{- enum {
    CLAP_AUDIO_PORT_IS_MAIN = 1 << 0,
    CLAP_AUDIO_PORT_SUPPORTS_64BITS = 1 << 1,
    CLAP_AUDIO_PORT_PREFERS_64BITS = 1 << 2,
    CLAP_AUDIO_PORT_REQUIRES_COMMON_SAMPLE_SIZE = 1 << 3
}; -}
c'CLAP_AUDIO_PORT_IS_MAIN = 1
c'CLAP_AUDIO_PORT_IS_MAIN :: (Num a) => a

{-# LINE 21 "src/Clap/Interface/Extension/Foreign/AudioPorts.hsc" #-}
c'CLAP_AUDIO_PORT_SUPPORTS_64BITS = 2
c'CLAP_AUDIO_PORT_SUPPORTS_64BITS :: (Num a) => a

{-# LINE 22 "src/Clap/Interface/Extension/Foreign/AudioPorts.hsc" #-}
c'CLAP_AUDIO_PORT_PREFERS_64BITS = 4
c'CLAP_AUDIO_PORT_PREFERS_64BITS :: (Num a) => a

{-# LINE 23 "src/Clap/Interface/Extension/Foreign/AudioPorts.hsc" #-}
c'CLAP_AUDIO_PORT_REQUIRES_COMMON_SAMPLE_SIZE = 8
c'CLAP_AUDIO_PORT_REQUIRES_COMMON_SAMPLE_SIZE :: (Num a) => a

{-# LINE 24 "src/Clap/Interface/Extension/Foreign/AudioPorts.hsc" #-}
{- typedef struct clap_audio_port_info {
            clap_id id;
            char name[CLAP_NAME_SIZE];
            uint32_t flags;
            uint32_t channel_count;
            const char * port_type;
            clap_id in_place_pair;
        } clap_audio_port_info_t; -}

{-# LINE 33 "src/Clap/Interface/Extension/Foreign/AudioPorts.hsc" #-}

{-# LINE 34 "src/Clap/Interface/Extension/Foreign/AudioPorts.hsc" #-}

{-# LINE 35 "src/Clap/Interface/Extension/Foreign/AudioPorts.hsc" #-}

{-# LINE 36 "src/Clap/Interface/Extension/Foreign/AudioPorts.hsc" #-}

{-# LINE 37 "src/Clap/Interface/Extension/Foreign/AudioPorts.hsc" #-}

{-# LINE 38 "src/Clap/Interface/Extension/Foreign/AudioPorts.hsc" #-}

{-# LINE 39 "src/Clap/Interface/Extension/Foreign/AudioPorts.hsc" #-}
data C'clap_audio_port_info = C'clap_audio_port_info{
  c'clap_audio_port_info'id :: CUInt,
  c'clap_audio_port_info'name :: [CChar],
  c'clap_audio_port_info'flags :: CUInt,
  c'clap_audio_port_info'channel_count :: CUInt,
  c'clap_audio_port_info'port_type :: CString,
  c'clap_audio_port_info'in_place_pair :: CUInt
} deriving (Eq,Show)
p'clap_audio_port_info'id p = plusPtr p 0
p'clap_audio_port_info'id :: Ptr (C'clap_audio_port_info) -> Ptr (CUInt)
p'clap_audio_port_info'name p = plusPtr p 4
p'clap_audio_port_info'name :: Ptr (C'clap_audio_port_info) -> Ptr (CChar)
p'clap_audio_port_info'flags p = plusPtr p 260
p'clap_audio_port_info'flags :: Ptr (C'clap_audio_port_info) -> Ptr (CUInt)
p'clap_audio_port_info'channel_count p = plusPtr p 264
p'clap_audio_port_info'channel_count :: Ptr (C'clap_audio_port_info) -> Ptr (CUInt)
p'clap_audio_port_info'port_type p = plusPtr p 272
p'clap_audio_port_info'port_type :: Ptr (C'clap_audio_port_info) -> Ptr (CString)
p'clap_audio_port_info'in_place_pair p = plusPtr p 280
p'clap_audio_port_info'in_place_pair :: Ptr (C'clap_audio_port_info) -> Ptr (CUInt)
instance Storable C'clap_audio_port_info where
  sizeOf _ = 288
  alignment _ = 8
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- let s1 = div 256 $ sizeOf $ (undefined :: CChar) in peekArray s1 (plusPtr _p 4)
    v2 <- peekByteOff _p 260
    v3 <- peekByteOff _p 264
    v4 <- peekByteOff _p 272
    v5 <- peekByteOff _p 280
    return $ C'clap_audio_port_info v0 v1 v2 v3 v4 v5
  poke _p (C'clap_audio_port_info v0 v1 v2 v3 v4 v5) = do
    pokeByteOff _p 0 v0
    let s1 = div 256 $ sizeOf $ (undefined :: CChar)
    pokeArray (plusPtr _p 4) (take s1 v1)
    pokeByteOff _p 260 v2
    pokeByteOff _p 264 v3
    pokeByteOff _p 272 v4
    pokeByteOff _p 280 v5
    return ()

{-# LINE 40 "src/Clap/Interface/Extension/Foreign/AudioPorts.hsc" #-}
type C'clap_audio_port_info_t = C'clap_audio_port_info

{-# LINE 41 "src/Clap/Interface/Extension/Foreign/AudioPorts.hsc" #-}
{- typedef struct clap_plugin_audio_ports {
            uint32_t (* count)(const clap_plugin_t * plugin, _Bool is_input);
            _Bool (* get)(const clap_plugin_t * plugin,
                          uint32_t index,
                          _Bool is_input,
                          clap_audio_port_info_t * info);
        } clap_plugin_audio_ports_t; -}

{-# LINE 49 "src/Clap/Interface/Extension/Foreign/AudioPorts.hsc" #-}

{-# LINE 50 "src/Clap/Interface/Extension/Foreign/AudioPorts.hsc" #-}

{-# LINE 51 "src/Clap/Interface/Extension/Foreign/AudioPorts.hsc" #-}
data C'clap_plugin_audio_ports = C'clap_plugin_audio_ports{
  c'clap_plugin_audio_ports'count :: FunPtr (Ptr C'clap_plugin -> CInt -> CUInt),
  c'clap_plugin_audio_ports'get :: FunPtr (Ptr C'clap_plugin -> CUInt -> CInt -> Ptr C'clap_audio_port_info -> CBool)
} deriving (Eq,Show)
p'clap_plugin_audio_ports'count p = plusPtr p 0
p'clap_plugin_audio_ports'count :: Ptr (C'clap_plugin_audio_ports) -> Ptr (FunPtr (Ptr C'clap_plugin -> CInt -> CUInt))
p'clap_plugin_audio_ports'get p = plusPtr p 8
p'clap_plugin_audio_ports'get :: Ptr (C'clap_plugin_audio_ports) -> Ptr (FunPtr (Ptr C'clap_plugin -> CUInt -> CInt -> Ptr C'clap_audio_port_info -> CBool))
instance Storable C'clap_plugin_audio_ports where
  sizeOf _ = 16
  alignment _ = 8
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 8
    return $ C'clap_plugin_audio_ports v0 v1
  poke _p (C'clap_plugin_audio_ports v0 v1) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 8 v1
    return ()

{-# LINE 52 "src/Clap/Interface/Extension/Foreign/AudioPorts.hsc" #-}
type C'clap_plugin_audio_ports_t = C'clap_plugin_audio_ports

{-# LINE 53 "src/Clap/Interface/Extension/Foreign/AudioPorts.hsc" #-}
type C'count = FunPtr (Ptr C'clap_plugin -> CInt -> CUInt)
foreign import ccall "wrapper" mk'count
  :: (Ptr C'clap_plugin -> CInt -> CUInt) -> IO C'count
foreign import ccall "dynamic" mK'count
  :: C'count -> (Ptr C'clap_plugin -> CInt -> CUInt)

{-# LINE 54 "src/Clap/Interface/Extension/Foreign/AudioPorts.hsc" #-}
type C'get = FunPtr (Ptr C'clap_plugin -> CUInt -> CInt -> Ptr C'clap_audio_port_info -> CBool)
foreign import ccall "wrapper" mk'get
  :: (Ptr C'clap_plugin -> CUInt -> CInt -> Ptr C'clap_audio_port_info -> CBool) -> IO C'get
foreign import ccall "dynamic" mK'get
  :: C'get -> (Ptr C'clap_plugin -> CUInt -> CInt -> Ptr C'clap_audio_port_info -> CBool)

{-# LINE 55 "src/Clap/Interface/Extension/Foreign/AudioPorts.hsc" #-}
{- enum {
    CLAP_AUDIO_PORTS_RESCAN_NAMES = 1 << 0,
    CLAP_AUDIO_PORTS_RESCAN_FLAGS = 1 << 1,
    CLAP_AUDIO_PORTS_RESCAN_CHANNEL_COUNT = 1 << 2,
    CLAP_AUDIO_PORTS_RESCAN_PORT_TYPE = 1 << 3,
    CLAP_AUDIO_PORTS_RESCAN_IN_PLACE_PAIR = 1 << 4,
    CLAP_AUDIO_PORTS_RESCAN_LIST = 1 << 5
}; -}
c'CLAP_AUDIO_PORTS_RESCAN_NAMES = 1
c'CLAP_AUDIO_PORTS_RESCAN_NAMES :: (Num a) => a

{-# LINE 64 "src/Clap/Interface/Extension/Foreign/AudioPorts.hsc" #-}
c'CLAP_AUDIO_PORTS_RESCAN_FLAGS = 2
c'CLAP_AUDIO_PORTS_RESCAN_FLAGS :: (Num a) => a

{-# LINE 65 "src/Clap/Interface/Extension/Foreign/AudioPorts.hsc" #-}
c'CLAP_AUDIO_PORTS_RESCAN_CHANNEL_COUNT = 4
c'CLAP_AUDIO_PORTS_RESCAN_CHANNEL_COUNT :: (Num a) => a

{-# LINE 66 "src/Clap/Interface/Extension/Foreign/AudioPorts.hsc" #-}
c'CLAP_AUDIO_PORTS_RESCAN_PORT_TYPE = 8
c'CLAP_AUDIO_PORTS_RESCAN_PORT_TYPE :: (Num a) => a

{-# LINE 67 "src/Clap/Interface/Extension/Foreign/AudioPorts.hsc" #-}
c'CLAP_AUDIO_PORTS_RESCAN_IN_PLACE_PAIR = 16
c'CLAP_AUDIO_PORTS_RESCAN_IN_PLACE_PAIR :: (Num a) => a

{-# LINE 68 "src/Clap/Interface/Extension/Foreign/AudioPorts.hsc" #-}
c'CLAP_AUDIO_PORTS_RESCAN_LIST = 32
c'CLAP_AUDIO_PORTS_RESCAN_LIST :: (Num a) => a

{-# LINE 69 "src/Clap/Interface/Extension/Foreign/AudioPorts.hsc" #-}
{- typedef struct clap_host_audio_ports {
            _Bool (* is_rescan_flag_supported)(const clap_host_t * host,
                                               uint32_t flag);
            void (* rescan)(const clap_host_t * host, uint32_t flags);
        } clap_host_audio_ports_t; -}

{-# LINE 75 "src/Clap/Interface/Extension/Foreign/AudioPorts.hsc" #-}

{-# LINE 76 "src/Clap/Interface/Extension/Foreign/AudioPorts.hsc" #-}

{-# LINE 77 "src/Clap/Interface/Extension/Foreign/AudioPorts.hsc" #-}
data C'clap_host_audio_ports = C'clap_host_audio_ports{
  c'clap_host_audio_ports'is_rescan_flag_supported :: FunPtr (Ptr C'clap_host -> CUInt -> CBool),
  c'clap_host_audio_ports'rescan :: FunPtr (Ptr C'clap_host -> CUInt -> IO ())
} deriving (Eq,Show)
p'clap_host_audio_ports'is_rescan_flag_supported p = plusPtr p 0
p'clap_host_audio_ports'is_rescan_flag_supported :: Ptr (C'clap_host_audio_ports) -> Ptr (FunPtr (Ptr C'clap_host -> CUInt -> CBool))
p'clap_host_audio_ports'rescan p = plusPtr p 8
p'clap_host_audio_ports'rescan :: Ptr (C'clap_host_audio_ports) -> Ptr (FunPtr (Ptr C'clap_host -> CUInt -> IO ()))
instance Storable C'clap_host_audio_ports where
  sizeOf _ = 16
  alignment _ = 8
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 8
    return $ C'clap_host_audio_ports v0 v1
  poke _p (C'clap_host_audio_ports v0 v1) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 8 v1
    return ()

{-# LINE 78 "src/Clap/Interface/Extension/Foreign/AudioPorts.hsc" #-}
type C'clap_host_audio_ports_t = C'clap_host_audio_ports

{-# LINE 79 "src/Clap/Interface/Extension/Foreign/AudioPorts.hsc" #-}
type C'is_rescan_flag_supported = FunPtr (Ptr C'clap_host -> CUInt -> CBool)
foreign import ccall "wrapper" mk'is_rescan_flag_supported
  :: (Ptr C'clap_host -> CUInt -> CBool) -> IO C'is_rescan_flag_supported
foreign import ccall "dynamic" mK'is_rescan_flag_supported
  :: C'is_rescan_flag_supported -> (Ptr C'clap_host -> CUInt -> CBool)

{-# LINE 80 "src/Clap/Interface/Extension/Foreign/AudioPorts.hsc" #-}
type C'rescan = FunPtr (Ptr C'clap_host -> CUInt -> IO ())
foreign import ccall "wrapper" mk'rescan
  :: (Ptr C'clap_host -> CUInt -> IO ()) -> IO C'rescan
foreign import ccall "dynamic" mK'rescan
  :: C'rescan -> (Ptr C'clap_host -> CUInt -> IO ())
