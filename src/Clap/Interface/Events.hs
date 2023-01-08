{-# LANGUAGE RankNTypes #-}

module Clap.Interface.Events where

import Clap.Interface.Foreign.Events
import Clap.Interface.Id

import Foreign.Ptr
import Foreign.Storable


newtype EventHandle = EventHandle { unEventHandle :: Ptr () }

data EventHeader = EventHeader
    { eventHeader_size :: Int
    , eventHeader_time :: Int
    , eventHeader_spaceId :: Int
    , eventHeader_type :: Int
    , eventHeader_flags :: Int
    }

data EventFlag 
    = IsLive
    | DoNotRecord 

-- TODO
createEvent :: Event -> Int -> Int -> [EventFlag] -> IO EventHandle
createEvent event time spaceId flas = pure undefined

data Event
    = NoteOn NoteEvent
    | NoteOff NoteEvent
    | NoteChoke NoteKillEvent
    | NoteEnd NoteKillEvent
    | NoteExpression ExpressionEvent
    | ParamValue ParamValueEvent
    | ParamMod ParamModEvent
    | ParamGestureBegin ParamGestureEvent
    | ParamGestureEnd ParamGestureEvent
    | Transport TransportEvent
    | Midi MidiEvent
    | MidiSysex MidiSysexEvent
    | Midi2 Midi2Event

data NoteEvent = NoteEvent
    { noteEvent_id :: Int
    , noteEvent_portIndex :: Int
    , noteEvent_channel :: Int
    , noteEvent_key :: Int
    , noteEvent_velocity :: Double
    }

data NoteKillEvent = NoteKillEvent
    { noteKillEvent_id :: Int
    , noteKillEvent_portIndex :: Int
    , noteKillEvent_channel :: Int
    , noteKillEvent_key :: Int
    }

data NoteExpression
    = Volume Double 
    | Pan Double
    | Tuning Double
    | Vibrato Double
    | Expression Double
    | Brightness Double
    | Pressure Double

data ExpressionEvent = ExpressionEvent
    { expressionEvent_noteId :: Int
    , expressionEvent_portIndex :: Int
    , expressionEvent_channel :: Int
    , expressionEvent_key :: Int
    , expressionEvent_value :: NoteExpression
    }

data ParamValueEvent = ParamValueEvent
    { paramValueEvent_paramId :: ClapId
    , paramValueEvent_cookie :: forall d. Storable d => d
    , paramValueEvent_noteId :: Int
    , paramValueEvent_portIndex :: Int
    , paramValueEvent_channel :: Int
    , paramValueEvent_key :: Int
    , paramValueEvent_value :: Double 
    }

data ParamModEvent = ParamModEvent
    { paramModEvent_paramId :: ClapId
    , paramModEvent_cookie :: forall d. Storable d => d
    , paramModEvent_noteId :: Int
    , paramModEvent_portIndex :: Int
    , paramModEvent_channel :: Int
    , paramModEvent_key :: Int
    , paramModEvent_amount :: Double 
    }

-- TODO
data ParamGestureEvent

-- TODO
data TransportEvent

-- TODO
data MidiEvent

-- TODO
data MidiSysexEvent

-- TODO
data Midi2Event