{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Clap.Interface.Events where

import Clap.Interface.Foreign
import Clap.Interface.Foreign.Events
import Clap.Interface.Fixedpoint
import Data.IORef
import Data.Int
import Data.Word
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import GHC.Stack

coreEventSpaceId :: Word16
coreEventSpaceId = 0

type EventHandle = Ptr C'clap_event_header

data EventFlag 
    = EventFlag_IsLive
    | EventFlag_DoNotRecord
    deriving (Enum, Bounded, Show)

data EventConfig = EventConfig
    { eventConfig_time :: Word32
    , eventConfig_spaceId :: Word16
    , eventConfig_flags :: [EventFlag] 
    } deriving (Show)

defaultEventConfig :: EventConfig 
defaultEventConfig = EventConfig
    { eventConfig_time = 0
    , eventConfig_spaceId = coreEventSpaceId
    , eventConfig_flags = [] 
    }

data Event
    = Event_NoteOn NoteEvent
    | Event_NoteOff NoteEvent
    | Event_NoteChoke NoteKillEvent
    | Event_NoteEnd NoteKillEvent
    | Event_NoteExpression NoteExpressionEvent
    | Event_ParamValue ParamValueEvent
    | Event_ParamMod ParamModEvent
    | Event_ParamGestureBegin ParamGestureEvent
    | Event_ParamGestureEnd ParamGestureEvent
    | Event_Transport TransportEvent
    | Event_Midi MidiEvent
    | Event_MidiSysex MidiSysexEvent
    | Event_Midi2 Midi2Event
    deriving (Show)

eventToEnumValue :: Event -> Int
eventToEnumValue = \case
    Event_NoteOn _ -> 0
    Event_NoteOff _ -> 1
    Event_NoteChoke _ -> 2
    Event_NoteEnd _ -> 3
    Event_NoteExpression _ -> 4
    Event_ParamValue _ -> 5
    Event_ParamMod _ -> 6
    Event_ParamGestureBegin _ -> 7
    Event_ParamGestureEnd _ -> 8
    Event_Transport _ -> 9
    Event_Midi _ -> 10 
    Event_MidiSysex _ -> 11
    Event_Midi2 _ -> 12

data NoteEvent = NoteEvent
    { noteEvent_noteId :: Int32
    , noteEvent_portIndex :: Int16
    , noteEvent_channel :: Int16
    , noteEvent_key :: Int16
    , noteEvent_velocity :: Double
    } deriving (Show)

data NoteKillEvent = NoteKillEvent
    { noteKillEvent_noteId :: Int32
    , noteKillEvent_portIndex :: Int16
    , noteKillEvent_channel :: Int16
    , noteKillEvent_key :: Int16
    } deriving (Show)

data NoteExpression
    = NoteExpression_Volume Double 
    | NoteExpression_Pan Double
    | NoteExpression_Tuning Double
    | NoteExpression_Vibrato Double
    | NoteExpression_Expression Double
    | NoteExpression_Brightness Double
    | NoteExpression_Pressure Double
    deriving (Show)

noteExpressionToEnumValue :: NoteExpression -> Int
noteExpressionToEnumValue = \case
    NoteExpression_Volume _ -> 0
    NoteExpression_Pan _ -> 1
    NoteExpression_Tuning _ -> 2
    NoteExpression_Vibrato _ -> 3
    NoteExpression_Expression _ -> 4
    NoteExpression_Brightness _ -> 5
    NoteExpression_Pressure _ -> 6

noteExpressionToDouble :: NoteExpression -> Double
noteExpressionToDouble = \case
    NoteExpression_Volume v -> v
    NoteExpression_Pan p -> p
    NoteExpression_Tuning t -> t
    NoteExpression_Vibrato v -> v
    NoteExpression_Expression e -> e
    NoteExpression_Brightness b -> b
    NoteExpression_Pressure p -> p

data NoteExpressionEvent = NoteExpressionEvent
    { noteExpressionEvent_noteId :: Int32
    , noteExpressionEvent_portIndex :: Int16
    , noteExpressionEvent_channel :: Int16
    , noteExpressionEvent_key :: Int16
    , noteExpressionEvent_value :: NoteExpression
    } deriving (Show)

newtype ParamId = ParamId { paramId_id :: Int }
    deriving (Show, Eq, Ord, Enum, Num, Real, Integral)

data ParamValueEvent = ParamValueEvent
    { paramValueEvent_paramId :: ParamId
    , paramValueEvent_cookie :: Ptr ()
    , paramValueEvent_noteId :: Int32
    , paramValueEvent_portIndex :: Int16
    , paramValueEvent_channel :: Int16
    , paramValueEvent_key :: Int16
    , paramValueEvent_value :: Double 
    } deriving (Show)

data ParamModEvent = ParamModEvent
    { paramModEvent_paramId :: ParamId
    , paramModEvent_cookie :: Ptr ()
    , paramModEvent_noteId :: Int32
    , paramModEvent_portIndex :: Int16
    , paramModEvent_channel :: Int16
    , paramModEvent_key :: Int16
    , paramModEvent_amount :: Double 
    } deriving (Show)

data ParamGestureEvent = ParamGestureEvent
    { paramGestureEvent_paramId :: ParamId }
    deriving (Show)

data TransportFlag
    = TransportFlag_HasTempo
    | TransportFlag_HasBeatsTimeline
    | TransportFlag_HasSecondsTimeline
    | TransportFlag_HasTimeSignature
    | TransportFlag_IsPlaying
    | TransportFlag_IsRecording
    | TransportFlag_IsLoopActive
    | TransportFlag_IsWithinPreRoll
    deriving (Enum, Bounded, Show)

data TransportEvent = TransportEvent
    { transportEvent_flags :: [TransportFlag]
    , transportEvent_songPositionBeats :: BeatTime
    , transportEvent_songPositionSeconds :: SecondsTime
    , transportEvent_tempo :: Double
    , transportEvent_tempoIncrement :: Double
    , transportEvent_loopStartBeats :: BeatTime
    , transportEvent_loopEndBeats :: BeatTime
    , transportEvent_loopStartSeconds :: SecondsTime
    , transportEvent_loopEndSeconds :: SecondsTime
    , transportEvent_barStart :: BeatTime
    , transportEvent_barNumber :: Int32
    , transportEvent_timeSignatureNumerator :: Word16 
    , transportEvent_timeSignatureDenominator :: Word16
    } deriving (Show)

data MidiData = MidiData
    { midiData_first :: Word8
    , midiData_second :: Word8
    , midiData_third :: Word8 
    } deriving (Show)

data MidiEvent = MidiEvent
    { midiEvent_portIndex :: Word16
    , midiEvent_data :: MidiData
    } deriving (Show)

data MidiSysexEvent = MidiSysexEvent
    { midiSysexEvent_portIndex :: Word16
    , midiSysexEvent_buffer :: [Word8] 
    } deriving (Show)

data Midi2Data = Midi2Data
    { midi2Data_first :: Word32
    , midi2Data_second :: Word32
    , midi2Data_third :: Word32
    , midi2Data_fourth :: Word32 
    } deriving (Show)

data Midi2Event = Midi2Event
    { midi2Event_portIndex :: Word16
    , midi2Event_data :: Midi2Data
    } deriving (Show)

type InputEventsHandle = Ptr C'clap_input_events

createInputEvents :: IORef [EventHandle] -> IO InputEventsHandle
createInputEvents events = do
    sizePtr <- mk'size $ size events
    getPtr <- mk'get $ get events
    new $ C'clap_input_events
        { c'clap_input_events'ctx = nullPtr
        , c'clap_input_events'size  = sizePtr
        , c'clap_input_events'get  = getPtr
        }

size :: IORef [EventHandle] -> InputEventsHandle -> IO CUInt
size events _ = fromIntegral . length <$> readIORef events
    
get :: IORef [EventHandle] -> InputEventsHandle -> CUInt -> IO (Ptr C'clap_event_header)
get events _ index = (!! fromIntegral index) <$> readIORef events

push :: IORef [EventHandle] -> EventConfig -> Event -> IO ()
push events eventConfig event = do
    newEvent <- createEvent eventConfig event
    modifyIORef' events (<> [newEvent]) 

readEvent :: EventHandle -> IO (Maybe Event)
readEvent cEventHeader = do
    eventType <- peek $ p'clap_event_header'type cEventHeader
    case eventType of
        0 -> do
            cNoteEvent <- peek $ castPtr cEventHeader
            pure $ Just . Event_NoteOn $ noteEventFromStruct cNoteEvent
        1 -> do
            cNoteEvent <- peek $ castPtr cEventHeader
            pure $ Just . Event_NoteOff $ noteEventFromStruct cNoteEvent
        2 -> do
            cNoteKillEvent <- peek $ castPtr cEventHeader
            pure $ Just . Event_NoteChoke $ noteKillEventFromStruct cNoteKillEvent
        3 -> do
            cNoteKillEvent <- peek $ castPtr cEventHeader
            pure $ Just . Event_NoteEnd $ noteKillEventFromStruct cNoteKillEvent
        4 -> do
            cNoteExpressionEvent <- peek $ castPtr cEventHeader
            pure $ Just . Event_NoteExpression $ noteExpressionEventFromStruct cNoteExpressionEvent
        5 -> do
            cParamValueEvent <- peek $ castPtr cEventHeader
            pure $ Just . Event_ParamValue $ paramValueEventFromStruct cParamValueEvent
        6 -> do
            cParamModEvent <- peek $ castPtr cEventHeader
            pure $ Just . Event_ParamMod $ paramModEventFromStruct cParamModEvent
        7 -> do
            cParamGestureEvent <- peek $ castPtr cEventHeader
            pure $ Just . Event_ParamGestureBegin $ paramGestureEventFromStruct cParamGestureEvent
        8 -> do
            cParamGestureEvent <- peek $ castPtr cEventHeader
            pure $ Just . Event_ParamGestureEnd $ paramGestureEventFromStruct cParamGestureEvent
        9 -> do
            cTransportEvent <- peek $ castPtr cEventHeader
            pure $ Just . Event_Transport $ transportEventFromStruct cTransportEvent
        10 -> do
            cMidiEvent <- peek $ castPtr cEventHeader
            pure $ Just . Event_Midi $ midiEventFromStruct cMidiEvent
        11 -> do
            cMidiSysexEvent <- peek $ castPtr cEventHeader
            Just . Event_MidiSysex <$> midiSysexEventFromStruct cMidiSysexEvent
        12 -> do
            cMidi2Event <- peek $ castPtr cEventHeader
            pure $ Just . Event_Midi2 $ midi2EventFromStruct cMidi2Event
        _ -> pure Nothing
    where

        noteEventFromStruct cNote = NoteEvent 
            { noteEvent_noteId = fromIntegral $ c'clap_event_note'note_id cNote
            , noteEvent_portIndex = fromIntegral $ c'clap_event_note'port_index cNote
            , noteEvent_channel = fromIntegral $ c'clap_event_note'channel cNote
            , noteEvent_key = fromIntegral $ c'clap_event_note'key cNote
            , noteEvent_velocity = fromCDouble $ c'clap_event_note'velocity cNote
            }

        noteKillEventFromStruct cNoteKill = NoteKillEvent 
            { noteKillEvent_noteId = fromIntegral $ c'clap_event_note'note_id cNoteKill
            , noteKillEvent_portIndex = fromIntegral $ c'clap_event_note'port_index cNoteKill
            , noteKillEvent_channel = fromIntegral $ c'clap_event_note'channel cNoteKill
            , noteKillEvent_key = fromIntegral $ c'clap_event_note'key cNoteKill
            }
                    
        noteExpressionEventFromStruct cNoteExpression = NoteExpressionEvent 
            { noteExpressionEvent_noteId = fromIntegral $ c'clap_event_note_expression'note_id cNoteExpression
            , noteExpressionEvent_portIndex = fromIntegral $ c'clap_event_note_expression'port_index cNoteExpression
            , noteExpressionEvent_channel = fromIntegral $ c'clap_event_note_expression'channel cNoteExpression
            , noteExpressionEvent_key = fromIntegral $ c'clap_event_note_expression'key cNoteExpression
            , noteExpressionEvent_value = 
                let constructor = 
                        case c'clap_event_note_expression'expression_id cNoteExpression of
                            0 -> NoteExpression_Volume 
                            1 -> NoteExpression_Pan
                            2 -> NoteExpression_Tuning
                            3 -> NoteExpression_Vibrato
                            4 -> NoteExpression_Expression
                            5 -> NoteExpression_Brightness
                            6 -> NoteExpression_Pressure
                            _ -> NoteExpression_Volume
                    CDouble value = c'clap_event_note_expression'value cNoteExpression
                in constructor value
            }

        paramValueEventFromStruct cParamValue = ParamValueEvent 
            { paramValueEvent_paramId = fromIntegral $ c'clap_event_param_value'param_id cParamValue
            , paramValueEvent_cookie = c'clap_event_param_value'cookie cParamValue
            , paramValueEvent_noteId = fromIntegral $ c'clap_event_param_value'note_id cParamValue
            , paramValueEvent_portIndex = fromIntegral $ c'clap_event_param_value'port_index cParamValue
            , paramValueEvent_channel = fromIntegral $ c'clap_event_param_value'channel cParamValue
            , paramValueEvent_key = fromIntegral $ c'clap_event_param_value'key cParamValue
            , paramValueEvent_value = fromCDouble $ c'clap_event_param_value'value cParamValue
            }

        paramModEventFromStruct cParamMod = ParamModEvent 
            { paramModEvent_paramId = fromIntegral $ c'clap_event_param_mod'param_id cParamMod
            , paramModEvent_cookie = c'clap_event_param_mod'cookie cParamMod
            , paramModEvent_noteId = fromIntegral $ c'clap_event_param_mod'note_id cParamMod
            , paramModEvent_portIndex = fromIntegral $ c'clap_event_param_mod'port_index cParamMod
            , paramModEvent_channel = fromIntegral $ c'clap_event_param_mod'channel cParamMod 
            , paramModEvent_key = fromIntegral $ c'clap_event_param_mod'key cParamMod
            , paramModEvent_amount = fromCDouble $ c'clap_event_param_mod'amount cParamMod
            }


        paramGestureEventFromStruct cParamGesture = ParamGestureEvent 
            { paramGestureEvent_paramId = fromIntegral $ c'clap_event_param_gesture'param_id cParamGesture }

        transportEventFromStruct cTransport = TransportEvent 
            { transportEvent_flags = intToFlags $ fromIntegral $ c'clap_event_transport'flags cTransport
            , transportEvent_songPositionBeats = fromIntegral $ c'clap_event_transport'song_pos_beats cTransport
            , transportEvent_songPositionSeconds = fromIntegral $ c'clap_event_transport'song_pos_seconds cTransport
            , transportEvent_tempo = fromCDouble $ c'clap_event_transport'tempo cTransport
            , transportEvent_tempoIncrement = fromCDouble $ c'clap_event_transport'tempo_inc cTransport
            , transportEvent_loopStartBeats = fromIntegral $ c'clap_event_transport'loop_start_beats cTransport
            , transportEvent_loopEndBeats = fromIntegral $ c'clap_event_transport'loop_end_beats cTransport
            , transportEvent_loopStartSeconds = fromIntegral $ c'clap_event_transport'loop_start_seconds cTransport
            , transportEvent_loopEndSeconds = fromIntegral $ c'clap_event_transport'loop_end_seconds cTransport
            , transportEvent_barStart = fromIntegral $ c'clap_event_transport'bar_start cTransport
            , transportEvent_barNumber = fromIntegral $ c'clap_event_transport'bar_number cTransport
            , transportEvent_timeSignatureNumerator = fromIntegral $ c'clap_event_transport'tsig_num cTransport
            , transportEvent_timeSignatureDenominator = fromIntegral $ c'clap_event_transport'tsig_denom cTransport
            }

        midiEventFromStruct cMidi = 
            let [first, second, third] = fromIntegral <$> c'clap_event_midi'data cMidi
            in MidiEvent 
                { midiEvent_portIndex = fromIntegral $ c'clap_event_midi'port_index cMidi
                , midiEvent_data = MidiData first second third
                }

        midiSysexEventFromStruct cMidiSysex = do
            buffer <- peekArray (fromIntegral $ c'clap_event_midi_sysex'size cMidiSysex) (c'clap_event_midi_sysex'buffer cMidiSysex)
            pure $ MidiSysexEvent 
                { midiSysexEvent_portIndex = fromIntegral $ c'clap_event_midi_sysex'port_index cMidiSysex
                , midiSysexEvent_buffer = fromIntegral <$> buffer
                }
            
        midi2EventFromStruct cMidi2 = 
            let [first, second, third, fourth] = fromIntegral <$> c'clap_event_midi2'data cMidi2
            in Midi2Event 
                { midi2Event_portIndex = fromIntegral $ c'clap_event_midi2'port_index cMidi2
                , midi2Event_data = Midi2Data first second third fourth
                }


type OutputEventsHandle = Ptr C'clap_output_events

createOutputEvents :: IO OutputEventsHandle
createOutputEvents = do
    listPtr <- newArray ([] :: [Ptr C'clap_event_header])
    tryPushPtr <- mk'try_push tryPush
    new $ C'clap_output_events
        { c'clap_output_events'ctx = castPtr listPtr
        , c'clap_output_events'try_push  = tryPushPtr
        }

tryPush :: HasCallStack => OutputEventsHandle -> Ptr C'clap_event_header -> IO CInt
tryPush outputEvents event = do
    funPtr <- peek $ p'clap_output_events'try_push outputEvents
    mK'try_push funPtr outputEvents event

createEvent :: EventConfig -> Event -> IO (Ptr C'clap_event_header)
createEvent eventConfig event = 
    case event of
        Event_NoteOn noteOn -> castPtr <$> new (noteEventToStruct noteOn)
        Event_NoteOff noteOff -> castPtr <$> new (noteEventToStruct noteOff)
        Event_NoteChoke noteChoke -> castPtr <$> new (noteKillEventToStruct noteChoke)
        Event_NoteEnd noteEnd -> castPtr <$> new (noteKillEventToStruct noteEnd)
        Event_NoteExpression noteExpression -> castPtr <$> new (noteExpressionEventToStruct noteExpression)
        Event_ParamValue paramValue -> castPtr <$> new (paramValueEventToStruct paramValue)
        Event_ParamMod paramMod -> castPtr <$> new (paramModEventToStruct paramMod)
        Event_ParamGestureBegin paramGesture -> castPtr <$> new (paramGestureEventToStruct paramGesture)
        Event_ParamGestureEnd paramGesture -> castPtr <$> new (paramGestureEventToStruct paramGesture)
        Event_Transport transport -> castPtr <$> new (transportEventToStruct transport)
        Event_Midi midi -> castPtr <$> new (midiEventToStruct midi)
        Event_MidiSysex midiSysex -> castPtr <$> (midiSysexEventToStruct midiSysex >>= new)
        Event_Midi2 midi2 -> castPtr <$> new (midi2EventToStruct midi2)
    
    where 
        noteEventToStruct note = C'clap_event_note
            { c'clap_event_note'header = eventToHeader event (undefined :: C'clap_event_note)
            , c'clap_event_note'note_id = fromIntegral $ noteEvent_noteId note
            , c'clap_event_note'port_index = fromIntegral $ noteEvent_portIndex note
            , c'clap_event_note'channel = fromIntegral $ noteEvent_channel note
            , c'clap_event_note'key = fromIntegral $ noteEvent_key note
            , c'clap_event_note'velocity = CDouble $ noteEvent_velocity note
            }
        noteKillEventToStruct noteKill = C'clap_event_note
            { c'clap_event_note'header = eventToHeader event (undefined :: C'clap_event_note)
            , c'clap_event_note'note_id = fromIntegral $ noteKillEvent_noteId noteKill
            , c'clap_event_note'port_index = fromIntegral $ noteKillEvent_portIndex noteKill
            , c'clap_event_note'channel = fromIntegral $ noteKillEvent_channel noteKill
            , c'clap_event_note'key = fromIntegral $ noteKillEvent_key noteKill
            , c'clap_event_note'velocity = 0
            }
        noteExpressionEventToStruct noteExpression = C'clap_event_note_expression
            { c'clap_event_note_expression'header = eventToHeader event (undefined :: C'clap_event_note_expression)
            , c'clap_event_note_expression'expression_id = fromIntegral $ noteExpressionToEnumValue (noteExpressionEvent_value noteExpression)
            , c'clap_event_note_expression'note_id = fromIntegral $ noteExpressionEvent_noteId noteExpression
            , c'clap_event_note_expression'port_index = fromIntegral $ noteExpressionEvent_portIndex noteExpression
            , c'clap_event_note_expression'channel = fromIntegral $ noteExpressionEvent_channel noteExpression
            , c'clap_event_note_expression'key = fromIntegral $ noteExpressionEvent_key noteExpression
            , c'clap_event_note_expression'value = CDouble $ noteExpressionToDouble $ noteExpressionEvent_value noteExpression
            }
        paramValueEventToStruct paramValue = C'clap_event_param_value
            { c'clap_event_param_value'header = eventToHeader event (undefined :: C'clap_event_param_value)
            , c'clap_event_param_value'param_id = fromIntegral $ paramValueEvent_paramId paramValue
            ,  c'clap_event_param_value'cookie = paramValueEvent_cookie paramValue
            , c'clap_event_param_value'note_id = fromIntegral $ paramValueEvent_noteId paramValue
            , c'clap_event_param_value'port_index = fromIntegral $ paramValueEvent_portIndex paramValue
            , c'clap_event_param_value'channel = fromIntegral $ paramValueEvent_channel paramValue
            , c'clap_event_param_value'key = fromIntegral $ paramValueEvent_key paramValue
            , c'clap_event_param_value'value = CDouble $ paramValueEvent_value paramValue
            }
        paramModEventToStruct paramMod = C'clap_event_param_mod
            { c'clap_event_param_mod'header = eventToHeader event (undefined :: C'clap_event_param_mod)
            , c'clap_event_param_mod'param_id = fromIntegral $ paramModEvent_paramId paramMod
            , c'clap_event_param_mod'cookie = paramModEvent_cookie paramMod
            , c'clap_event_param_mod'note_id = fromIntegral $ paramModEvent_noteId paramMod
            , c'clap_event_param_mod'port_index = fromIntegral $ paramModEvent_portIndex paramMod
            , c'clap_event_param_mod'channel = fromIntegral $ paramModEvent_channel paramMod
            , c'clap_event_param_mod'key = fromIntegral $ paramModEvent_key paramMod
            , c'clap_event_param_mod'amount = CDouble $ paramModEvent_amount paramMod
            }
        paramGestureEventToStruct paramGesture = C'clap_event_param_gesture
            { c'clap_event_param_gesture'header = eventToHeader event (undefined :: C'clap_event_param_mod)
            , c'clap_event_param_gesture'param_id = fromIntegral $ paramGestureEvent_paramId paramGesture
            }
        transportEventToStruct transport = C'clap_event_transport
            { c'clap_event_transport'header = eventToHeader event (undefined :: C'clap_event_transport)
            , c'clap_event_transport'flags = fromIntegral $ flagsToInt $ transportEvent_flags transport
            , c'clap_event_transport'song_pos_beats = fromIntegral $ transportEvent_songPositionBeats transport
            , c'clap_event_transport'song_pos_seconds = fromIntegral $ transportEvent_songPositionSeconds transport
            , c'clap_event_transport'tempo = CDouble $ transportEvent_tempo transport
            , c'clap_event_transport'tempo_inc = CDouble $ transportEvent_tempoIncrement transport
            , c'clap_event_transport'loop_start_beats = fromIntegral $ transportEvent_loopStartBeats transport
            , c'clap_event_transport'loop_end_beats = fromIntegral $ transportEvent_loopEndBeats transport
            , c'clap_event_transport'loop_start_seconds = fromIntegral $ transportEvent_loopStartSeconds transport
            , c'clap_event_transport'loop_end_seconds = fromIntegral $ transportEvent_loopEndSeconds transport
            , c'clap_event_transport'bar_start = fromIntegral $ transportEvent_barStart transport
            , c'clap_event_transport'bar_number = fromIntegral $ transportEvent_barNumber transport
            , c'clap_event_transport'tsig_num = fromIntegral $ transportEvent_timeSignatureNumerator transport
            , c'clap_event_transport'tsig_denom = fromIntegral $ transportEvent_timeSignatureDenominator transport
            }
        midiEventToStruct midi = C'clap_event_midi
            { c'clap_event_midi'header = eventToHeader event (undefined :: C'clap_event_midi)
            , c'clap_event_midi'port_index = fromIntegral $ midiEvent_portIndex midi
            , c'clap_event_midi'data = 
                let MidiData first second third = midiEvent_data midi
                in fromIntegral <$> [first, second, third]
            }
        midiSysexEventToStruct midiSysex = do
            cBuffer <- newArray $ CUChar <$> midiSysexEvent_buffer midiSysex
            pure $ C'clap_event_midi_sysex
                { c'clap_event_midi_sysex'header = eventToHeader event (undefined :: C'clap_event_midi_sysex)
                , c'clap_event_midi_sysex'port_index = fromIntegral $ midiSysexEvent_portIndex midiSysex
                , c'clap_event_midi_sysex'buffer = cBuffer
                , c'clap_event_midi_sysex'size = fromIntegral $ length $ midiSysexEvent_buffer midiSysex
                }
        midi2EventToStruct midi2 = C'clap_event_midi2
            { c'clap_event_midi2'header = eventToHeader event (undefined :: C'clap_event_midi2)
            , c'clap_event_midi2'port_index = fromIntegral $ midi2Event_portIndex midi2
            , c'clap_event_midi2'data = 
                let Midi2Data first second third fourth = midi2Event_data midi2
                in fromIntegral <$> [first, second, third, fourth]
            }

        eventToHeader event cEvent = C'clap_event_header 
            { c'clap_event_header'size = fromIntegral $ sizeOf cEvent
            , c'clap_event_header'time = fromIntegral $ eventConfig_time eventConfig
            , c'clap_event_header'space_id = fromIntegral $ eventConfig_spaceId eventConfig
            , c'clap_event_header'type = fromIntegral $ eventToEnumValue event
            , c'clap_event_header'flags = fromIntegral $ flagsToInt $ eventConfig_flags eventConfig
            } 