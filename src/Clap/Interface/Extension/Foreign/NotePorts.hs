{-# LINE 1 "src/Clap/Interface/Extension/Foreign/NotePorts.hsc" #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}


module Clap.Interface.Extension.Foreign.NotePorts where
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

{-# LINE 7 "src/Clap/Interface/Extension/Foreign/NotePorts.hsc" #-}

import Clap.Interface.Foreign.Host
import Clap.Interface.Foreign.Plugin
import Clap.Interface.StringSizes
-- #globalarray CLAP_EXT_NOTE_PORTS , CChar
{- enum clap_note_dialect {
    CLAP_NOTE_DIALECT_CLAP = 1 << 0,
    CLAP_NOTE_DIALECT_MIDI = 1 << 1,
    CLAP_NOTE_DIALECT_MIDI_MPE = 1 << 2,
    CLAP_NOTE_DIALECT_MIDI2 = 1 << 3
}; -}
type C'clap_note_dialect = CUInt

{-# LINE 19 "src/Clap/Interface/Extension/Foreign/NotePorts.hsc" #-}
c'CLAP_NOTE_DIALECT_CLAP = 1
c'CLAP_NOTE_DIALECT_CLAP :: (Num a) => a

{-# LINE 20 "src/Clap/Interface/Extension/Foreign/NotePorts.hsc" #-}
c'CLAP_NOTE_DIALECT_MIDI = 2
c'CLAP_NOTE_DIALECT_MIDI :: (Num a) => a

{-# LINE 21 "src/Clap/Interface/Extension/Foreign/NotePorts.hsc" #-}
c'CLAP_NOTE_DIALECT_MIDI_MPE = 4
c'CLAP_NOTE_DIALECT_MIDI_MPE :: (Num a) => a

{-# LINE 22 "src/Clap/Interface/Extension/Foreign/NotePorts.hsc" #-}
c'CLAP_NOTE_DIALECT_MIDI2 = 8
c'CLAP_NOTE_DIALECT_MIDI2 :: (Num a) => a

{-# LINE 23 "src/Clap/Interface/Extension/Foreign/NotePorts.hsc" #-}
{- typedef struct clap_note_port_info {
            clap_id id;
            uint32_t supported_dialects;
            uint32_t preferred_dialect;
            char name[CLAP_NAME_SIZE];
        } clap_note_port_info_t; -}

{-# LINE 30 "src/Clap/Interface/Extension/Foreign/NotePorts.hsc" #-}

{-# LINE 31 "src/Clap/Interface/Extension/Foreign/NotePorts.hsc" #-}

{-# LINE 32 "src/Clap/Interface/Extension/Foreign/NotePorts.hsc" #-}

{-# LINE 33 "src/Clap/Interface/Extension/Foreign/NotePorts.hsc" #-}

{-# LINE 34 "src/Clap/Interface/Extension/Foreign/NotePorts.hsc" #-}
data C'clap_note_port_info = C'clap_note_port_info{
  c'clap_note_port_info'id :: CUInt,
  c'clap_note_port_info'supported_dialects :: CUInt,
  c'clap_note_port_info'preferred_dialect :: CUInt,
  c'clap_note_port_info'name :: [CChar]
} deriving (Eq,Show)
p'clap_note_port_info'id p = plusPtr p 0
p'clap_note_port_info'id :: Ptr (C'clap_note_port_info) -> Ptr (CUInt)
p'clap_note_port_info'supported_dialects p = plusPtr p 4
p'clap_note_port_info'supported_dialects :: Ptr (C'clap_note_port_info) -> Ptr (CUInt)
p'clap_note_port_info'preferred_dialect p = plusPtr p 8
p'clap_note_port_info'preferred_dialect :: Ptr (C'clap_note_port_info) -> Ptr (CUInt)
p'clap_note_port_info'name p = plusPtr p 12
p'clap_note_port_info'name :: Ptr (C'clap_note_port_info) -> Ptr (CChar)
instance Storable C'clap_note_port_info where
  sizeOf _ = 268
  alignment _ = 4
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 4
    v2 <- peekByteOff _p 8
    v3 <- let s3 = div 256 $ sizeOf $ (undefined :: CChar) in peekArray s3 (plusPtr _p 12)
    return $ C'clap_note_port_info v0 v1 v2 v3
  poke _p (C'clap_note_port_info v0 v1 v2 v3) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 4 v1
    pokeByteOff _p 8 v2
    let s3 = div 256 $ sizeOf $ (undefined :: CChar)
    pokeArray (plusPtr _p 12) (take s3 v3)
    return ()

{-# LINE 35 "src/Clap/Interface/Extension/Foreign/NotePorts.hsc" #-}
type C'clap_note_port_info_t = C'clap_note_port_info

{-# LINE 36 "src/Clap/Interface/Extension/Foreign/NotePorts.hsc" #-}
{- typedef struct clap_plugin_note_ports {
            uint32_t (* count)(const clap_plugin_t * plugin, _Bool is_input);
            _Bool (* get)(const clap_plugin_t * plugin,
                          uint32_t index,
                          _Bool is_input,
                          clap_note_port_info_t * info);
        } clap_plugin_note_ports_t; -}

{-# LINE 44 "src/Clap/Interface/Extension/Foreign/NotePorts.hsc" #-}

{-# LINE 45 "src/Clap/Interface/Extension/Foreign/NotePorts.hsc" #-}

{-# LINE 46 "src/Clap/Interface/Extension/Foreign/NotePorts.hsc" #-}
data C'clap_plugin_note_ports = C'clap_plugin_note_ports{
  c'clap_plugin_note_ports'count :: FunPtr (Ptr C'clap_plugin -> CBool -> CUInt),
  c'clap_plugin_note_ports'get :: FunPtr (Ptr C'clap_plugin -> CUInt -> CInt -> Ptr C'clap_note_port_info -> CBool)
} deriving (Eq,Show)
p'clap_plugin_note_ports'count p = plusPtr p 0
p'clap_plugin_note_ports'count :: Ptr (C'clap_plugin_note_ports) -> Ptr (FunPtr (Ptr C'clap_plugin -> CBool -> CUInt))
p'clap_plugin_note_ports'get p = plusPtr p 8
p'clap_plugin_note_ports'get :: Ptr (C'clap_plugin_note_ports) -> Ptr (FunPtr (Ptr C'clap_plugin -> CUInt -> CInt -> Ptr C'clap_note_port_info -> CBool))
instance Storable C'clap_plugin_note_ports where
  sizeOf _ = 16
  alignment _ = 8
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 8
    return $ C'clap_plugin_note_ports v0 v1
  poke _p (C'clap_plugin_note_ports v0 v1) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 8 v1
    return ()

{-# LINE 47 "src/Clap/Interface/Extension/Foreign/NotePorts.hsc" #-}
type C'clap_plugin_note_ports_t = C'clap_plugin_note_ports

{-# LINE 48 "src/Clap/Interface/Extension/Foreign/NotePorts.hsc" #-}
type C'count = FunPtr (Ptr C'clap_plugin -> CBool -> CUInt)
foreign import ccall "wrapper" mk'count
  :: (Ptr C'clap_plugin -> CBool -> CUInt) -> IO C'count
foreign import ccall "dynamic" mK'count
  :: C'count -> (Ptr C'clap_plugin -> CBool -> CUInt)

{-# LINE 49 "src/Clap/Interface/Extension/Foreign/NotePorts.hsc" #-}
type C'get = FunPtr (Ptr C'clap_plugin -> CUInt -> CInt -> Ptr C'clap_note_port_info -> CBool)
foreign import ccall "wrapper" mk'get
  :: (Ptr C'clap_plugin -> CUInt -> CInt -> Ptr C'clap_note_port_info -> CBool) -> IO C'get
foreign import ccall "dynamic" mK'get
  :: C'get -> (Ptr C'clap_plugin -> CUInt -> CInt -> Ptr C'clap_note_port_info -> CBool)

{-# LINE 50 "src/Clap/Interface/Extension/Foreign/NotePorts.hsc" #-}
{- enum {
    CLAP_NOTE_PORTS_RESCAN_ALL = 1 << 0,
    CLAP_NOTE_PORTS_RESCAN_NAMES = 1 << 1
}; -}
c'CLAP_NOTE_PORTS_RESCAN_ALL = 1
c'CLAP_NOTE_PORTS_RESCAN_ALL :: (Num a) => a

{-# LINE 55 "src/Clap/Interface/Extension/Foreign/NotePorts.hsc" #-}
c'CLAP_NOTE_PORTS_RESCAN_NAMES = 2
c'CLAP_NOTE_PORTS_RESCAN_NAMES :: (Num a) => a

{-# LINE 56 "src/Clap/Interface/Extension/Foreign/NotePorts.hsc" #-}
{- typedef struct clap_host_note_ports {
            uint32_t (* supported_dialects)(const clap_host_t * host);
            void (* rescan)(const clap_host_t * host, uint32_t flags);
        } clap_host_note_ports_t; -}

{-# LINE 61 "src/Clap/Interface/Extension/Foreign/NotePorts.hsc" #-}

{-# LINE 62 "src/Clap/Interface/Extension/Foreign/NotePorts.hsc" #-}

{-# LINE 63 "src/Clap/Interface/Extension/Foreign/NotePorts.hsc" #-}
data C'clap_host_note_ports = C'clap_host_note_ports{
  c'clap_host_note_ports'supported_dialects :: FunPtr (Ptr C'clap_host -> CUInt),
  c'clap_host_note_ports'rescan :: FunPtr (Ptr C'clap_host -> CUInt -> IO ())
} deriving (Eq,Show)
p'clap_host_note_ports'supported_dialects p = plusPtr p 0
p'clap_host_note_ports'supported_dialects :: Ptr (C'clap_host_note_ports) -> Ptr (FunPtr (Ptr C'clap_host -> CUInt))
p'clap_host_note_ports'rescan p = plusPtr p 8
p'clap_host_note_ports'rescan :: Ptr (C'clap_host_note_ports) -> Ptr (FunPtr (Ptr C'clap_host -> CUInt -> IO ()))
instance Storable C'clap_host_note_ports where
  sizeOf _ = 16
  alignment _ = 8
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 8
    return $ C'clap_host_note_ports v0 v1
  poke _p (C'clap_host_note_ports v0 v1) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 8 v1
    return ()

{-# LINE 64 "src/Clap/Interface/Extension/Foreign/NotePorts.hsc" #-}
type C'clap_host_note_ports_t = C'clap_host_note_ports

{-# LINE 65 "src/Clap/Interface/Extension/Foreign/NotePorts.hsc" #-}
type C'supported_dialects = FunPtr (Ptr C'clap_host -> CUInt)
foreign import ccall "wrapper" mk'supported_dialects
  :: (Ptr C'clap_host -> CUInt) -> IO C'supported_dialects
foreign import ccall "dynamic" mK'supported_dialects
  :: C'supported_dialects -> (Ptr C'clap_host -> CUInt)

{-# LINE 66 "src/Clap/Interface/Extension/Foreign/NotePorts.hsc" #-}
type C'rescan = FunPtr (Ptr C'clap_host -> CUInt -> IO ())
foreign import ccall "wrapper" mk'rescan
  :: (Ptr C'clap_host -> CUInt -> IO ()) -> IO C'rescan
foreign import ccall "dynamic" mK'rescan
  :: C'rescan -> (Ptr C'clap_host -> CUInt -> IO ())
