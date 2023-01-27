{-# LINE 1 "src/Clap/Interface/Foreign/Events.hsc" #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}


module Clap.Interface.Foreign.Events where
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

{-# LINE 7 "src/Clap/Interface/Foreign/Events.hsc" #-}

import Clap.Interface.Foreign.Fixedpoint
import Clap.Interface.Foreign.Id
{- typedef struct clap_event_header {
            uint32_t size;
            uint32_t time;
            uint16_t space_id;
            uint16_t type;
            uint32_t flags;
        } clap_event_header_t; -}

{-# LINE 18 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 19 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 20 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 21 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 22 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 23 "src/Clap/Interface/Foreign/Events.hsc" #-}
data C'clap_event_header = C'clap_event_header{
  c'clap_event_header'size :: CUInt,
  c'clap_event_header'time :: CUInt,
  c'clap_event_header'space_id :: CUShort,
  c'clap_event_header'type :: CUShort,
  c'clap_event_header'flags :: CUInt
} deriving (Eq,Show)
p'clap_event_header'size p = plusPtr p 0
p'clap_event_header'size :: Ptr (C'clap_event_header) -> Ptr (CUInt)
p'clap_event_header'time p = plusPtr p 4
p'clap_event_header'time :: Ptr (C'clap_event_header) -> Ptr (CUInt)
p'clap_event_header'space_id p = plusPtr p 8
p'clap_event_header'space_id :: Ptr (C'clap_event_header) -> Ptr (CUShort)
p'clap_event_header'type p = plusPtr p 10
p'clap_event_header'type :: Ptr (C'clap_event_header) -> Ptr (CUShort)
p'clap_event_header'flags p = plusPtr p 12
p'clap_event_header'flags :: Ptr (C'clap_event_header) -> Ptr (CUInt)
instance Storable C'clap_event_header where
  sizeOf _ = 16
  alignment _ = 4
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 4
    v2 <- peekByteOff _p 8
    v3 <- peekByteOff _p 10
    v4 <- peekByteOff _p 12
    return $ C'clap_event_header v0 v1 v2 v3 v4
  poke _p (C'clap_event_header v0 v1 v2 v3 v4) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 4 v1
    pokeByteOff _p 8 v2
    pokeByteOff _p 10 v3
    pokeByteOff _p 12 v4
    return ()

{-# LINE 24 "src/Clap/Interface/Foreign/Events.hsc" #-}
type C'clap_event_header_t = C'clap_event_header

{-# LINE 25 "src/Clap/Interface/Foreign/Events.hsc" #-}
{- enum clap_event_flags {
    CLAP_EVENT_IS_LIVE = 1 << 0, CLAP_EVENT_DONT_RECORD = 1 << 1
}; -}
type C'clap_event_flags = CUInt

{-# LINE 29 "src/Clap/Interface/Foreign/Events.hsc" #-}
c'CLAP_EVENT_IS_LIVE = 1
c'CLAP_EVENT_IS_LIVE :: (Num a) => a

{-# LINE 30 "src/Clap/Interface/Foreign/Events.hsc" #-}
c'CLAP_EVENT_DONT_RECORD = 2
c'CLAP_EVENT_DONT_RECORD :: (Num a) => a

{-# LINE 31 "src/Clap/Interface/Foreign/Events.hsc" #-}
{- enum {
    CLAP_EVENT_NOTE_ON,
    CLAP_EVENT_NOTE_OFF,
    CLAP_EVENT_NOTE_CHOKE,
    CLAP_EVENT_NOTE_END,
    CLAP_EVENT_NOTE_EXPRESSION,
    CLAP_EVENT_PARAM_VALUE,
    CLAP_EVENT_PARAM_MOD,
    CLAP_EVENT_PARAM_GESTURE_BEGIN,
    CLAP_EVENT_PARAM_GESTURE_END,
    CLAP_EVENT_TRANSPORT,
    CLAP_EVENT_MIDI,
    CLAP_EVENT_MIDI_SYSEX,
    CLAP_EVENT_MIDI2
}; -}
c'CLAP_EVENT_NOTE_ON = 0
c'CLAP_EVENT_NOTE_ON :: (Num a) => a

{-# LINE 47 "src/Clap/Interface/Foreign/Events.hsc" #-}
c'CLAP_EVENT_NOTE_OFF = 1
c'CLAP_EVENT_NOTE_OFF :: (Num a) => a

{-# LINE 48 "src/Clap/Interface/Foreign/Events.hsc" #-}
c'CLAP_EVENT_NOTE_CHOKE = 2
c'CLAP_EVENT_NOTE_CHOKE :: (Num a) => a

{-# LINE 49 "src/Clap/Interface/Foreign/Events.hsc" #-}
c'CLAP_EVENT_NOTE_END = 3
c'CLAP_EVENT_NOTE_END :: (Num a) => a

{-# LINE 50 "src/Clap/Interface/Foreign/Events.hsc" #-}
c'CLAP_EVENT_NOTE_EXPRESSION = 4
c'CLAP_EVENT_NOTE_EXPRESSION :: (Num a) => a

{-# LINE 51 "src/Clap/Interface/Foreign/Events.hsc" #-}
c'CLAP_EVENT_PARAM_VALUE = 5
c'CLAP_EVENT_PARAM_VALUE :: (Num a) => a

{-# LINE 52 "src/Clap/Interface/Foreign/Events.hsc" #-}
c'CLAP_EVENT_PARAM_MOD = 6
c'CLAP_EVENT_PARAM_MOD :: (Num a) => a

{-# LINE 53 "src/Clap/Interface/Foreign/Events.hsc" #-}
c'CLAP_EVENT_PARAM_GESTURE_BEGIN = 7
c'CLAP_EVENT_PARAM_GESTURE_BEGIN :: (Num a) => a

{-# LINE 54 "src/Clap/Interface/Foreign/Events.hsc" #-}
c'CLAP_EVENT_PARAM_GESTURE_END = 8
c'CLAP_EVENT_PARAM_GESTURE_END :: (Num a) => a

{-# LINE 55 "src/Clap/Interface/Foreign/Events.hsc" #-}
c'CLAP_EVENT_TRANSPORT = 9
c'CLAP_EVENT_TRANSPORT :: (Num a) => a

{-# LINE 56 "src/Clap/Interface/Foreign/Events.hsc" #-}
c'CLAP_EVENT_MIDI = 10
c'CLAP_EVENT_MIDI :: (Num a) => a

{-# LINE 57 "src/Clap/Interface/Foreign/Events.hsc" #-}
c'CLAP_EVENT_MIDI_SYSEX = 11
c'CLAP_EVENT_MIDI_SYSEX :: (Num a) => a

{-# LINE 58 "src/Clap/Interface/Foreign/Events.hsc" #-}
c'CLAP_EVENT_MIDI2 = 12
c'CLAP_EVENT_MIDI2 :: (Num a) => a

{-# LINE 59 "src/Clap/Interface/Foreign/Events.hsc" #-}
{- typedef struct clap_event_note {
            clap_event_header_t header;
            int32_t note_id;
            int16_t port_index;
            int16_t channel;
            int16_t key;
            double velocity;
        } clap_event_note_t; -}

{-# LINE 68 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 69 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 70 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 71 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 72 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 73 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 74 "src/Clap/Interface/Foreign/Events.hsc" #-}
data C'clap_event_note = C'clap_event_note{
  c'clap_event_note'header :: C'clap_event_header,
  c'clap_event_note'note_id :: CInt,
  c'clap_event_note'port_index :: CShort,
  c'clap_event_note'channel :: CShort,
  c'clap_event_note'key :: CShort,
  c'clap_event_note'velocity :: CDouble
} deriving (Eq,Show)
p'clap_event_note'header p = plusPtr p 0
p'clap_event_note'header :: Ptr (C'clap_event_note) -> Ptr (C'clap_event_header)
p'clap_event_note'note_id p = plusPtr p 16
p'clap_event_note'note_id :: Ptr (C'clap_event_note) -> Ptr (CInt)
p'clap_event_note'port_index p = plusPtr p 20
p'clap_event_note'port_index :: Ptr (C'clap_event_note) -> Ptr (CShort)
p'clap_event_note'channel p = plusPtr p 22
p'clap_event_note'channel :: Ptr (C'clap_event_note) -> Ptr (CShort)
p'clap_event_note'key p = plusPtr p 24
p'clap_event_note'key :: Ptr (C'clap_event_note) -> Ptr (CShort)
p'clap_event_note'velocity p = plusPtr p 32
p'clap_event_note'velocity :: Ptr (C'clap_event_note) -> Ptr (CDouble)
instance Storable C'clap_event_note where
  sizeOf _ = 40
  alignment _ = 8
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 16
    v2 <- peekByteOff _p 20
    v3 <- peekByteOff _p 22
    v4 <- peekByteOff _p 24
    v5 <- peekByteOff _p 32
    return $ C'clap_event_note v0 v1 v2 v3 v4 v5
  poke _p (C'clap_event_note v0 v1 v2 v3 v4 v5) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 16 v1
    pokeByteOff _p 20 v2
    pokeByteOff _p 22 v3
    pokeByteOff _p 24 v4
    pokeByteOff _p 32 v5
    return ()

{-# LINE 75 "src/Clap/Interface/Foreign/Events.hsc" #-}
type C'clap_event_note_t = C'clap_event_note

{-# LINE 76 "src/Clap/Interface/Foreign/Events.hsc" #-}
{- enum {
    CLAP_NOTE_EXPRESSION_VOLUME,
    CLAP_NOTE_EXPRESSION_PAN,
    CLAP_NOTE_EXPRESSION_TUNING,
    CLAP_NOTE_EXPRESSION_VIBRATO,
    CLAP_NOTE_EXPRESSION_EXPRESSION,
    CLAP_NOTE_EXPRESSION_BRIGHTNESS,
    CLAP_NOTE_EXPRESSION_PRESSURE
}; -}
c'CLAP_NOTE_EXPRESSION_VOLUME = 0
c'CLAP_NOTE_EXPRESSION_VOLUME :: (Num a) => a

{-# LINE 86 "src/Clap/Interface/Foreign/Events.hsc" #-}
c'CLAP_NOTE_EXPRESSION_PAN = 1
c'CLAP_NOTE_EXPRESSION_PAN :: (Num a) => a

{-# LINE 87 "src/Clap/Interface/Foreign/Events.hsc" #-}
c'CLAP_NOTE_EXPRESSION_TUNING = 2
c'CLAP_NOTE_EXPRESSION_TUNING :: (Num a) => a

{-# LINE 88 "src/Clap/Interface/Foreign/Events.hsc" #-}
c'CLAP_NOTE_EXPRESSION_VIBRATO = 3
c'CLAP_NOTE_EXPRESSION_VIBRATO :: (Num a) => a

{-# LINE 89 "src/Clap/Interface/Foreign/Events.hsc" #-}
c'CLAP_NOTE_EXPRESSION_EXPRESSION = 4
c'CLAP_NOTE_EXPRESSION_EXPRESSION :: (Num a) => a

{-# LINE 90 "src/Clap/Interface/Foreign/Events.hsc" #-}
c'CLAP_NOTE_EXPRESSION_BRIGHTNESS = 5
c'CLAP_NOTE_EXPRESSION_BRIGHTNESS :: (Num a) => a

{-# LINE 91 "src/Clap/Interface/Foreign/Events.hsc" #-}
c'CLAP_NOTE_EXPRESSION_PRESSURE = 6
c'CLAP_NOTE_EXPRESSION_PRESSURE :: (Num a) => a

{-# LINE 92 "src/Clap/Interface/Foreign/Events.hsc" #-}
{- typedef int32_t clap_note_expression; -}
type C'clap_note_expression = CInt

{-# LINE 94 "src/Clap/Interface/Foreign/Events.hsc" #-}
{- typedef struct clap_event_note_expression {
            clap_event_header_t header;
            clap_note_expression expression_id;
            int32_t note_id;
            int16_t port_index;
            int16_t channel;
            int16_t key;
            double value;
        } clap_event_note_expression_t; -}

{-# LINE 104 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 105 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 106 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 107 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 108 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 109 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 110 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 111 "src/Clap/Interface/Foreign/Events.hsc" #-}
data C'clap_event_note_expression = C'clap_event_note_expression{
  c'clap_event_note_expression'header :: C'clap_event_header,
  c'clap_event_note_expression'expression_id :: CInt,
  c'clap_event_note_expression'note_id :: CInt,
  c'clap_event_note_expression'port_index :: CShort,
  c'clap_event_note_expression'channel :: CShort,
  c'clap_event_note_expression'key :: CShort,
  c'clap_event_note_expression'value :: CDouble
} deriving (Eq,Show)
p'clap_event_note_expression'header p = plusPtr p 0
p'clap_event_note_expression'header :: Ptr (C'clap_event_note_expression) -> Ptr (C'clap_event_header)
p'clap_event_note_expression'expression_id p = plusPtr p 16
p'clap_event_note_expression'expression_id :: Ptr (C'clap_event_note_expression) -> Ptr (CInt)
p'clap_event_note_expression'note_id p = plusPtr p 20
p'clap_event_note_expression'note_id :: Ptr (C'clap_event_note_expression) -> Ptr (CInt)
p'clap_event_note_expression'port_index p = plusPtr p 24
p'clap_event_note_expression'port_index :: Ptr (C'clap_event_note_expression) -> Ptr (CShort)
p'clap_event_note_expression'channel p = plusPtr p 26
p'clap_event_note_expression'channel :: Ptr (C'clap_event_note_expression) -> Ptr (CShort)
p'clap_event_note_expression'key p = plusPtr p 28
p'clap_event_note_expression'key :: Ptr (C'clap_event_note_expression) -> Ptr (CShort)
p'clap_event_note_expression'value p = plusPtr p 32
p'clap_event_note_expression'value :: Ptr (C'clap_event_note_expression) -> Ptr (CDouble)
instance Storable C'clap_event_note_expression where
  sizeOf _ = 40
  alignment _ = 8
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 16
    v2 <- peekByteOff _p 20
    v3 <- peekByteOff _p 24
    v4 <- peekByteOff _p 26
    v5 <- peekByteOff _p 28
    v6 <- peekByteOff _p 32
    return $ C'clap_event_note_expression v0 v1 v2 v3 v4 v5 v6
  poke _p (C'clap_event_note_expression v0 v1 v2 v3 v4 v5 v6) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 16 v1
    pokeByteOff _p 20 v2
    pokeByteOff _p 24 v3
    pokeByteOff _p 26 v4
    pokeByteOff _p 28 v5
    pokeByteOff _p 32 v6
    return ()

{-# LINE 112 "src/Clap/Interface/Foreign/Events.hsc" #-}
type C'clap_event_note_expression_t = C'clap_event_note_expression

{-# LINE 113 "src/Clap/Interface/Foreign/Events.hsc" #-}
{- typedef struct clap_event_param_value {
            clap_event_header_t header;
            clap_id param_id;
            void * cookie;
            int32_t note_id;
            int16_t port_index;
            int16_t channel;
            int16_t key;
            double value;
        } clap_event_param_value_t; -}

{-# LINE 124 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 125 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 126 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 127 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 128 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 129 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 130 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 131 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 132 "src/Clap/Interface/Foreign/Events.hsc" #-}
data C'clap_event_param_value = C'clap_event_param_value{
  c'clap_event_param_value'header :: C'clap_event_header,
  c'clap_event_param_value'param_id :: CUInt,
  c'clap_event_param_value'cookie :: Ptr (),
  c'clap_event_param_value'note_id :: CInt,
  c'clap_event_param_value'port_index :: CShort,
  c'clap_event_param_value'channel :: CShort,
  c'clap_event_param_value'key :: CShort,
  c'clap_event_param_value'value :: CDouble
} deriving (Eq,Show)
p'clap_event_param_value'header p = plusPtr p 0
p'clap_event_param_value'header :: Ptr (C'clap_event_param_value) -> Ptr (C'clap_event_header)
p'clap_event_param_value'param_id p = plusPtr p 16
p'clap_event_param_value'param_id :: Ptr (C'clap_event_param_value) -> Ptr (CUInt)
p'clap_event_param_value'cookie p = plusPtr p 24
p'clap_event_param_value'cookie :: Ptr (C'clap_event_param_value) -> Ptr (Ptr ())
p'clap_event_param_value'note_id p = plusPtr p 32
p'clap_event_param_value'note_id :: Ptr (C'clap_event_param_value) -> Ptr (CInt)
p'clap_event_param_value'port_index p = plusPtr p 36
p'clap_event_param_value'port_index :: Ptr (C'clap_event_param_value) -> Ptr (CShort)
p'clap_event_param_value'channel p = plusPtr p 38
p'clap_event_param_value'channel :: Ptr (C'clap_event_param_value) -> Ptr (CShort)
p'clap_event_param_value'key p = plusPtr p 40
p'clap_event_param_value'key :: Ptr (C'clap_event_param_value) -> Ptr (CShort)
p'clap_event_param_value'value p = plusPtr p 48
p'clap_event_param_value'value :: Ptr (C'clap_event_param_value) -> Ptr (CDouble)
instance Storable C'clap_event_param_value where
  sizeOf _ = 56
  alignment _ = 8
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 16
    v2 <- peekByteOff _p 24
    v3 <- peekByteOff _p 32
    v4 <- peekByteOff _p 36
    v5 <- peekByteOff _p 38
    v6 <- peekByteOff _p 40
    v7 <- peekByteOff _p 48
    return $ C'clap_event_param_value v0 v1 v2 v3 v4 v5 v6 v7
  poke _p (C'clap_event_param_value v0 v1 v2 v3 v4 v5 v6 v7) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 16 v1
    pokeByteOff _p 24 v2
    pokeByteOff _p 32 v3
    pokeByteOff _p 36 v4
    pokeByteOff _p 38 v5
    pokeByteOff _p 40 v6
    pokeByteOff _p 48 v7
    return ()

{-# LINE 133 "src/Clap/Interface/Foreign/Events.hsc" #-}
type C'clap_event_param_value_t = C'clap_event_param_value

{-# LINE 134 "src/Clap/Interface/Foreign/Events.hsc" #-}
{- typedef struct clap_event_param_mod {
            clap_event_header_t header;
            clap_id param_id;
            void * cookie;
            int32_t note_id;
            int16_t port_index;
            int16_t channel;
            int16_t key;
            double amount;
        } clap_event_param_mod_t; -}

{-# LINE 145 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 146 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 147 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 148 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 149 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 150 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 151 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 152 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 153 "src/Clap/Interface/Foreign/Events.hsc" #-}
data C'clap_event_param_mod = C'clap_event_param_mod{
  c'clap_event_param_mod'header :: C'clap_event_header,
  c'clap_event_param_mod'param_id :: CUInt,
  c'clap_event_param_mod'cookie :: Ptr (),
  c'clap_event_param_mod'note_id :: CInt,
  c'clap_event_param_mod'port_index :: CShort,
  c'clap_event_param_mod'channel :: CShort,
  c'clap_event_param_mod'key :: CShort,
  c'clap_event_param_mod'amount :: CDouble
} deriving (Eq,Show)
p'clap_event_param_mod'header p = plusPtr p 0
p'clap_event_param_mod'header :: Ptr (C'clap_event_param_mod) -> Ptr (C'clap_event_header)
p'clap_event_param_mod'param_id p = plusPtr p 16
p'clap_event_param_mod'param_id :: Ptr (C'clap_event_param_mod) -> Ptr (CUInt)
p'clap_event_param_mod'cookie p = plusPtr p 24
p'clap_event_param_mod'cookie :: Ptr (C'clap_event_param_mod) -> Ptr (Ptr ())
p'clap_event_param_mod'note_id p = plusPtr p 32
p'clap_event_param_mod'note_id :: Ptr (C'clap_event_param_mod) -> Ptr (CInt)
p'clap_event_param_mod'port_index p = plusPtr p 36
p'clap_event_param_mod'port_index :: Ptr (C'clap_event_param_mod) -> Ptr (CShort)
p'clap_event_param_mod'channel p = plusPtr p 38
p'clap_event_param_mod'channel :: Ptr (C'clap_event_param_mod) -> Ptr (CShort)
p'clap_event_param_mod'key p = plusPtr p 40
p'clap_event_param_mod'key :: Ptr (C'clap_event_param_mod) -> Ptr (CShort)
p'clap_event_param_mod'amount p = plusPtr p 48
p'clap_event_param_mod'amount :: Ptr (C'clap_event_param_mod) -> Ptr (CDouble)
instance Storable C'clap_event_param_mod where
  sizeOf _ = 56
  alignment _ = 8
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 16
    v2 <- peekByteOff _p 24
    v3 <- peekByteOff _p 32
    v4 <- peekByteOff _p 36
    v5 <- peekByteOff _p 38
    v6 <- peekByteOff _p 40
    v7 <- peekByteOff _p 48
    return $ C'clap_event_param_mod v0 v1 v2 v3 v4 v5 v6 v7
  poke _p (C'clap_event_param_mod v0 v1 v2 v3 v4 v5 v6 v7) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 16 v1
    pokeByteOff _p 24 v2
    pokeByteOff _p 32 v3
    pokeByteOff _p 36 v4
    pokeByteOff _p 38 v5
    pokeByteOff _p 40 v6
    pokeByteOff _p 48 v7
    return ()

{-# LINE 154 "src/Clap/Interface/Foreign/Events.hsc" #-}
type C'clap_event_param_mod_t = C'clap_event_param_mod

{-# LINE 155 "src/Clap/Interface/Foreign/Events.hsc" #-}
{- typedef struct clap_event_param_gesture {
            clap_event_header_t header; clap_id param_id;
        } clap_event_param_gesture_t; -}

{-# LINE 159 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 160 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 161 "src/Clap/Interface/Foreign/Events.hsc" #-}
data C'clap_event_param_gesture = C'clap_event_param_gesture{
  c'clap_event_param_gesture'header :: C'clap_event_header,
  c'clap_event_param_gesture'param_id :: CUInt
} deriving (Eq,Show)
p'clap_event_param_gesture'header p = plusPtr p 0
p'clap_event_param_gesture'header :: Ptr (C'clap_event_param_gesture) -> Ptr (C'clap_event_header)
p'clap_event_param_gesture'param_id p = plusPtr p 16
p'clap_event_param_gesture'param_id :: Ptr (C'clap_event_param_gesture) -> Ptr (CUInt)
instance Storable C'clap_event_param_gesture where
  sizeOf _ = 20
  alignment _ = 4
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 16
    return $ C'clap_event_param_gesture v0 v1
  poke _p (C'clap_event_param_gesture v0 v1) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 16 v1
    return ()

{-# LINE 162 "src/Clap/Interface/Foreign/Events.hsc" #-}
type C'clap_event_param_gesture_t = C'clap_event_param_gesture

{-# LINE 163 "src/Clap/Interface/Foreign/Events.hsc" #-}
{- enum clap_transport_flags {
    CLAP_TRANSPORT_HAS_TEMPO = 1 << 0,
    CLAP_TRANSPORT_HAS_BEATS_TIMELINE = 1 << 1,
    CLAP_TRANSPORT_HAS_SECONDS_TIMELINE = 1 << 2,
    CLAP_TRANSPORT_HAS_TIME_SIGNATURE = 1 << 3,
    CLAP_TRANSPORT_IS_PLAYING = 1 << 4,
    CLAP_TRANSPORT_IS_RECORDING = 1 << 5,
    CLAP_TRANSPORT_IS_LOOP_ACTIVE = 1 << 6,
    CLAP_TRANSPORT_IS_WITHIN_PRE_ROLL = 1 << 7
}; -}
type C'clap_transport_flags = CUInt

{-# LINE 174 "src/Clap/Interface/Foreign/Events.hsc" #-}
c'CLAP_TRANSPORT_HAS_TEMPO = 1
c'CLAP_TRANSPORT_HAS_TEMPO :: (Num a) => a

{-# LINE 175 "src/Clap/Interface/Foreign/Events.hsc" #-}
c'CLAP_TRANSPORT_HAS_BEATS_TIMELINE = 2
c'CLAP_TRANSPORT_HAS_BEATS_TIMELINE :: (Num a) => a

{-# LINE 176 "src/Clap/Interface/Foreign/Events.hsc" #-}
c'CLAP_TRANSPORT_HAS_SECONDS_TIMELINE = 4
c'CLAP_TRANSPORT_HAS_SECONDS_TIMELINE :: (Num a) => a

{-# LINE 177 "src/Clap/Interface/Foreign/Events.hsc" #-}
c'CLAP_TRANSPORT_HAS_TIME_SIGNATURE = 8
c'CLAP_TRANSPORT_HAS_TIME_SIGNATURE :: (Num a) => a

{-# LINE 178 "src/Clap/Interface/Foreign/Events.hsc" #-}
c'CLAP_TRANSPORT_IS_PLAYING = 16
c'CLAP_TRANSPORT_IS_PLAYING :: (Num a) => a

{-# LINE 179 "src/Clap/Interface/Foreign/Events.hsc" #-}
c'CLAP_TRANSPORT_IS_RECORDING = 32
c'CLAP_TRANSPORT_IS_RECORDING :: (Num a) => a

{-# LINE 180 "src/Clap/Interface/Foreign/Events.hsc" #-}
c'CLAP_TRANSPORT_IS_LOOP_ACTIVE = 64
c'CLAP_TRANSPORT_IS_LOOP_ACTIVE :: (Num a) => a

{-# LINE 181 "src/Clap/Interface/Foreign/Events.hsc" #-}
c'CLAP_TRANSPORT_IS_WITHIN_PRE_ROLL = 128
c'CLAP_TRANSPORT_IS_WITHIN_PRE_ROLL :: (Num a) => a

{-# LINE 182 "src/Clap/Interface/Foreign/Events.hsc" #-}
{- typedef struct clap_event_transport {
            clap_event_header_t header;
            uint32_t flags;
            clap_beattime song_pos_beats;
            clap_sectime song_pos_seconds;
            double tempo;
            double tempo_inc;
            clap_beattime loop_start_beats;
            clap_beattime loop_end_beats;
            clap_sectime loop_start_seconds;
            clap_sectime loop_end_seconds;
            clap_beattime bar_start;
            int32_t bar_number;
            uint16_t tsig_num;
            uint16_t tsig_denom;
        } clap_event_transport_t; -}

{-# LINE 199 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 200 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 201 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 202 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 203 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 204 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 205 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 206 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 207 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 208 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 209 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 210 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 211 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 212 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 213 "src/Clap/Interface/Foreign/Events.hsc" #-}
data C'clap_event_transport = C'clap_event_transport{
  c'clap_event_transport'header :: C'clap_event_header,
  c'clap_event_transport'flags :: CUInt,
  c'clap_event_transport'song_pos_beats :: CLong,
  c'clap_event_transport'song_pos_seconds :: CLong,
  c'clap_event_transport'tempo :: CDouble,
  c'clap_event_transport'tempo_inc :: CDouble,
  c'clap_event_transport'loop_start_beats :: CLong,
  c'clap_event_transport'loop_end_beats :: CLong,
  c'clap_event_transport'loop_start_seconds :: CLong,
  c'clap_event_transport'loop_end_seconds :: CLong,
  c'clap_event_transport'bar_start :: CLong,
  c'clap_event_transport'bar_number :: CInt,
  c'clap_event_transport'tsig_num :: CUShort,
  c'clap_event_transport'tsig_denom :: CUShort
} deriving (Eq,Show)
p'clap_event_transport'header p = plusPtr p 0
p'clap_event_transport'header :: Ptr (C'clap_event_transport) -> Ptr (C'clap_event_header)
p'clap_event_transport'flags p = plusPtr p 16
p'clap_event_transport'flags :: Ptr (C'clap_event_transport) -> Ptr (CUInt)
p'clap_event_transport'song_pos_beats p = plusPtr p 24
p'clap_event_transport'song_pos_beats :: Ptr (C'clap_event_transport) -> Ptr (CLong)
p'clap_event_transport'song_pos_seconds p = plusPtr p 32
p'clap_event_transport'song_pos_seconds :: Ptr (C'clap_event_transport) -> Ptr (CLong)
p'clap_event_transport'tempo p = plusPtr p 40
p'clap_event_transport'tempo :: Ptr (C'clap_event_transport) -> Ptr (CDouble)
p'clap_event_transport'tempo_inc p = plusPtr p 48
p'clap_event_transport'tempo_inc :: Ptr (C'clap_event_transport) -> Ptr (CDouble)
p'clap_event_transport'loop_start_beats p = plusPtr p 56
p'clap_event_transport'loop_start_beats :: Ptr (C'clap_event_transport) -> Ptr (CLong)
p'clap_event_transport'loop_end_beats p = plusPtr p 64
p'clap_event_transport'loop_end_beats :: Ptr (C'clap_event_transport) -> Ptr (CLong)
p'clap_event_transport'loop_start_seconds p = plusPtr p 72
p'clap_event_transport'loop_start_seconds :: Ptr (C'clap_event_transport) -> Ptr (CLong)
p'clap_event_transport'loop_end_seconds p = plusPtr p 80
p'clap_event_transport'loop_end_seconds :: Ptr (C'clap_event_transport) -> Ptr (CLong)
p'clap_event_transport'bar_start p = plusPtr p 88
p'clap_event_transport'bar_start :: Ptr (C'clap_event_transport) -> Ptr (CLong)
p'clap_event_transport'bar_number p = plusPtr p 96
p'clap_event_transport'bar_number :: Ptr (C'clap_event_transport) -> Ptr (CInt)
p'clap_event_transport'tsig_num p = plusPtr p 100
p'clap_event_transport'tsig_num :: Ptr (C'clap_event_transport) -> Ptr (CUShort)
p'clap_event_transport'tsig_denom p = plusPtr p 102
p'clap_event_transport'tsig_denom :: Ptr (C'clap_event_transport) -> Ptr (CUShort)
instance Storable C'clap_event_transport where
  sizeOf _ = 104
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
    v10 <- peekByteOff _p 88
    v11 <- peekByteOff _p 96
    v12 <- peekByteOff _p 100
    v13 <- peekByteOff _p 102
    return $ C'clap_event_transport v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13
  poke _p (C'clap_event_transport v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13) = do
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
    pokeByteOff _p 88 v10
    pokeByteOff _p 96 v11
    pokeByteOff _p 100 v12
    pokeByteOff _p 102 v13
    return ()

{-# LINE 214 "src/Clap/Interface/Foreign/Events.hsc" #-}
type C'clap_event_transport_t = C'clap_event_transport

{-# LINE 215 "src/Clap/Interface/Foreign/Events.hsc" #-}
{- typedef struct clap_event_midi {
            clap_event_header_t header; uint16_t port_index; uint8_t data[3];
        } clap_event_midi_t; -}

{-# LINE 219 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 220 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 221 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 222 "src/Clap/Interface/Foreign/Events.hsc" #-}
data C'clap_event_midi = C'clap_event_midi{
  c'clap_event_midi'header :: C'clap_event_header,
  c'clap_event_midi'port_index :: CUShort,
  c'clap_event_midi'data :: [CUChar]
} deriving (Eq,Show)
p'clap_event_midi'header p = plusPtr p 0
p'clap_event_midi'header :: Ptr (C'clap_event_midi) -> Ptr (C'clap_event_header)
p'clap_event_midi'port_index p = plusPtr p 16
p'clap_event_midi'port_index :: Ptr (C'clap_event_midi) -> Ptr (CUShort)
p'clap_event_midi'data p = plusPtr p 18
p'clap_event_midi'data :: Ptr (C'clap_event_midi) -> Ptr (CUChar)
instance Storable C'clap_event_midi where
  sizeOf _ = 24
  alignment _ = 4
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 16
    v2 <- let s2 = div 3 $ sizeOf $ (undefined :: CUChar) in peekArray s2 (plusPtr _p 18)
    return $ C'clap_event_midi v0 v1 v2
  poke _p (C'clap_event_midi v0 v1 v2) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 16 v1
    let s2 = div 3 $ sizeOf $ (undefined :: CUChar)
    pokeArray (plusPtr _p 18) (take s2 v2)
    return ()

{-# LINE 223 "src/Clap/Interface/Foreign/Events.hsc" #-}
type C'clap_event_midi_t = C'clap_event_midi

{-# LINE 224 "src/Clap/Interface/Foreign/Events.hsc" #-}
{- typedef struct clap_event_midi_sysex {
            clap_event_header_t header;
            uint16_t port_index;
            const uint8_t * buffer;
            uint32_t size;
        } clap_event_midi_sysex_t; -}

{-# LINE 231 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 232 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 233 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 234 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 235 "src/Clap/Interface/Foreign/Events.hsc" #-}
data C'clap_event_midi_sysex = C'clap_event_midi_sysex{
  c'clap_event_midi_sysex'header :: C'clap_event_header,
  c'clap_event_midi_sysex'port_index :: CUShort,
  c'clap_event_midi_sysex'buffer :: Ptr CUChar,
  c'clap_event_midi_sysex'size :: CUInt
} deriving (Eq,Show)
p'clap_event_midi_sysex'header p = plusPtr p 0
p'clap_event_midi_sysex'header :: Ptr (C'clap_event_midi_sysex) -> Ptr (C'clap_event_header)
p'clap_event_midi_sysex'port_index p = plusPtr p 16
p'clap_event_midi_sysex'port_index :: Ptr (C'clap_event_midi_sysex) -> Ptr (CUShort)
p'clap_event_midi_sysex'buffer p = plusPtr p 24
p'clap_event_midi_sysex'buffer :: Ptr (C'clap_event_midi_sysex) -> Ptr (Ptr CUChar)
p'clap_event_midi_sysex'size p = plusPtr p 32
p'clap_event_midi_sysex'size :: Ptr (C'clap_event_midi_sysex) -> Ptr (CUInt)
instance Storable C'clap_event_midi_sysex where
  sizeOf _ = 40
  alignment _ = 8
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 16
    v2 <- peekByteOff _p 24
    v3 <- peekByteOff _p 32
    return $ C'clap_event_midi_sysex v0 v1 v2 v3
  poke _p (C'clap_event_midi_sysex v0 v1 v2 v3) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 16 v1
    pokeByteOff _p 24 v2
    pokeByteOff _p 32 v3
    return ()

{-# LINE 236 "src/Clap/Interface/Foreign/Events.hsc" #-}
type C'clap_event_midi_sysex_t = C'clap_event_midi_sysex

{-# LINE 237 "src/Clap/Interface/Foreign/Events.hsc" #-}
{- typedef struct clap_event_midi2 {
            clap_event_header_t header; uint16_t port_index; uint32_t data[4];
        } clap_event_midi2_t; -}

{-# LINE 241 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 242 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 243 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 244 "src/Clap/Interface/Foreign/Events.hsc" #-}
data C'clap_event_midi2 = C'clap_event_midi2{
  c'clap_event_midi2'header :: C'clap_event_header,
  c'clap_event_midi2'port_index :: CUShort,
  c'clap_event_midi2'data :: [CUInt]
} deriving (Eq,Show)
p'clap_event_midi2'header p = plusPtr p 0
p'clap_event_midi2'header :: Ptr (C'clap_event_midi2) -> Ptr (C'clap_event_header)
p'clap_event_midi2'port_index p = plusPtr p 16
p'clap_event_midi2'port_index :: Ptr (C'clap_event_midi2) -> Ptr (CUShort)
p'clap_event_midi2'data p = plusPtr p 20
p'clap_event_midi2'data :: Ptr (C'clap_event_midi2) -> Ptr (CUInt)
instance Storable C'clap_event_midi2 where
  sizeOf _ = 36
  alignment _ = 4
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 16
    v2 <- let s2 = div 16 $ sizeOf $ (undefined :: CUInt) in peekArray s2 (plusPtr _p 20)
    return $ C'clap_event_midi2 v0 v1 v2
  poke _p (C'clap_event_midi2 v0 v1 v2) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 16 v1
    let s2 = div 16 $ sizeOf $ (undefined :: CUInt)
    pokeArray (plusPtr _p 20) (take s2 v2)
    return ()

{-# LINE 245 "src/Clap/Interface/Foreign/Events.hsc" #-}
type C'clap_event_midi2_t = C'clap_event_midi2

{-# LINE 246 "src/Clap/Interface/Foreign/Events.hsc" #-}
{- typedef struct clap_input_events {
            void * ctx;
            uint32_t (* size)(const struct clap_input_events * list);
            const clap_event_header_t * (* get)(const struct clap_input_events * list,
                                                uint32_t index);
        } clap_input_events_t; -}

{-# LINE 253 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 254 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 255 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 256 "src/Clap/Interface/Foreign/Events.hsc" #-}
data C'clap_input_events = C'clap_input_events{
  c'clap_input_events'ctx :: Ptr (),
  c'clap_input_events'size :: FunPtr (Ptr C'clap_input_events -> IO CUInt),
  c'clap_input_events'get :: FunPtr (Ptr C'clap_input_events -> CUInt -> IO (Ptr C'clap_event_header))
} deriving (Eq,Show)
p'clap_input_events'ctx p = plusPtr p 0
p'clap_input_events'ctx :: Ptr (C'clap_input_events) -> Ptr (Ptr ())
p'clap_input_events'size p = plusPtr p 8
p'clap_input_events'size :: Ptr (C'clap_input_events) -> Ptr (FunPtr (Ptr C'clap_input_events -> IO CUInt))
p'clap_input_events'get p = plusPtr p 16
p'clap_input_events'get :: Ptr (C'clap_input_events) -> Ptr (FunPtr (Ptr C'clap_input_events -> CUInt -> IO (Ptr C'clap_event_header)))
instance Storable C'clap_input_events where
  sizeOf _ = 24
  alignment _ = 8
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 8
    v2 <- peekByteOff _p 16
    return $ C'clap_input_events v0 v1 v2
  poke _p (C'clap_input_events v0 v1 v2) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 8 v1
    pokeByteOff _p 16 v2
    return ()

{-# LINE 257 "src/Clap/Interface/Foreign/Events.hsc" #-}
type C'clap_input_events_t = C'clap_input_events

{-# LINE 258 "src/Clap/Interface/Foreign/Events.hsc" #-}
type C'size = FunPtr (Ptr C'clap_input_events -> IO CUInt)
foreign import ccall "wrapper" mk'size
  :: (Ptr C'clap_input_events -> IO CUInt) -> IO C'size
foreign import ccall "dynamic" mK'size
  :: C'size -> (Ptr C'clap_input_events -> IO CUInt)

{-# LINE 259 "src/Clap/Interface/Foreign/Events.hsc" #-}
type C'get = FunPtr (Ptr C'clap_input_events -> CUInt -> IO (Ptr C'clap_event_header))
foreign import ccall "wrapper" mk'get
  :: (Ptr C'clap_input_events -> CUInt -> IO (Ptr C'clap_event_header)) -> IO C'get
foreign import ccall "dynamic" mK'get
  :: C'get -> (Ptr C'clap_input_events -> CUInt -> IO (Ptr C'clap_event_header))

{-# LINE 260 "src/Clap/Interface/Foreign/Events.hsc" #-}
{- typedef struct clap_output_events {
            void * ctx;
            _Bool (* try_push)(const struct clap_output_events * list,
                               const clap_event_header_t * event);
        } clap_output_events_t; -}

{-# LINE 266 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 267 "src/Clap/Interface/Foreign/Events.hsc" #-}

{-# LINE 268 "src/Clap/Interface/Foreign/Events.hsc" #-}
data C'clap_output_events = C'clap_output_events{
  c'clap_output_events'ctx :: Ptr (),
  c'clap_output_events'try_push :: FunPtr (Ptr C'clap_output_events -> Ptr C'clap_event_header -> IO CInt)
} deriving (Eq,Show)
p'clap_output_events'ctx p = plusPtr p 0
p'clap_output_events'ctx :: Ptr (C'clap_output_events) -> Ptr (Ptr ())
p'clap_output_events'try_push p = plusPtr p 8
p'clap_output_events'try_push :: Ptr (C'clap_output_events) -> Ptr (FunPtr (Ptr C'clap_output_events -> Ptr C'clap_event_header -> IO CInt))
instance Storable C'clap_output_events where
  sizeOf _ = 16
  alignment _ = 8
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 8
    return $ C'clap_output_events v0 v1
  poke _p (C'clap_output_events v0 v1) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 8 v1
    return ()

{-# LINE 269 "src/Clap/Interface/Foreign/Events.hsc" #-}
type C'clap_output_events_t = C'clap_output_events

{-# LINE 270 "src/Clap/Interface/Foreign/Events.hsc" #-}
type C'try_push = FunPtr (Ptr C'clap_output_events -> Ptr C'clap_event_header -> IO CInt)
foreign import ccall "wrapper" mk'try_push
  :: (Ptr C'clap_output_events -> Ptr C'clap_event_header -> IO CInt) -> IO C'try_push
foreign import ccall "dynamic" mK'try_push
  :: C'try_push -> (Ptr C'clap_output_events -> Ptr C'clap_event_header -> IO CInt)

{-# LINE 271 "src/Clap/Interface/Foreign/Events.hsc" #-}
