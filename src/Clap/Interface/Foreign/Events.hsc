{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <clap/events.h>
module Clap.Interface.Foreign.Events where
import Foreign.Ptr
#strict_import

import Clap.Interface.Foreign.Fixedpoint
import Clap.Interface.Foreign.Id
{- typedef struct clap_event_header {
            uint32_t size;
            uint32_t time;
            uint16_t space_id;
            uint16_t type;
            uint32_t flags;
        } clap_event_header_t; -}
#starttype struct clap_event_header
#field size , CUInt
#field time , CUInt
#field space_id , CUShort
#field type , CUShort
#field flags , CUInt
#stoptype
#synonym_t clap_event_header_t , <struct clap_event_header>
{- enum clap_event_flags {
    CLAP_EVENT_IS_LIVE = 1 << 0, CLAP_EVENT_DONT_RECORD = 1 << 1
}; -}
#integral_t enum clap_event_flags
#num CLAP_EVENT_IS_LIVE
#num CLAP_EVENT_DONT_RECORD
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
#num CLAP_EVENT_NOTE_ON
#num CLAP_EVENT_NOTE_OFF
#num CLAP_EVENT_NOTE_CHOKE
#num CLAP_EVENT_NOTE_END
#num CLAP_EVENT_NOTE_EXPRESSION
#num CLAP_EVENT_PARAM_VALUE
#num CLAP_EVENT_PARAM_MOD
#num CLAP_EVENT_PARAM_GESTURE_BEGIN
#num CLAP_EVENT_PARAM_GESTURE_END
#num CLAP_EVENT_TRANSPORT
#num CLAP_EVENT_MIDI
#num CLAP_EVENT_MIDI_SYSEX
#num CLAP_EVENT_MIDI2
{- typedef struct clap_event_note {
            clap_event_header_t header;
            int32_t note_id;
            int16_t port_index;
            int16_t channel;
            int16_t key;
            double velocity;
        } clap_event_note_t; -}
#starttype struct clap_event_note
#field header , <struct clap_event_header>
#field note_id , CInt
#field port_index , CShort
#field channel , CShort
#field key , CShort
#field velocity , CDouble
#stoptype
#synonym_t clap_event_note_t , <struct clap_event_note>
{- enum {
    CLAP_NOTE_EXPRESSION_VOLUME,
    CLAP_NOTE_EXPRESSION_PAN,
    CLAP_NOTE_EXPRESSION_TUNING,
    CLAP_NOTE_EXPRESSION_VIBRATO,
    CLAP_NOTE_EXPRESSION_EXPRESSION,
    CLAP_NOTE_EXPRESSION_BRIGHTNESS,
    CLAP_NOTE_EXPRESSION_PRESSURE
}; -}
#num CLAP_NOTE_EXPRESSION_VOLUME
#num CLAP_NOTE_EXPRESSION_PAN
#num CLAP_NOTE_EXPRESSION_TUNING
#num CLAP_NOTE_EXPRESSION_VIBRATO
#num CLAP_NOTE_EXPRESSION_EXPRESSION
#num CLAP_NOTE_EXPRESSION_BRIGHTNESS
#num CLAP_NOTE_EXPRESSION_PRESSURE
{- typedef int32_t clap_note_expression; -}
#synonym_t clap_note_expression , CInt
{- typedef struct clap_event_note_expression {
            clap_event_header_t header;
            clap_note_expression expression_id;
            int32_t note_id;
            int16_t port_index;
            int16_t channel;
            int16_t key;
            double value;
        } clap_event_note_expression_t; -}
#starttype struct clap_event_note_expression
#field header , <struct clap_event_header>
#field expression_id , CInt
#field note_id , CInt
#field port_index , CShort
#field channel , CShort
#field key , CShort
#field value , CDouble
#stoptype
#synonym_t clap_event_note_expression_t , <struct clap_event_note_expression>
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
#starttype struct clap_event_param_value
#field header , <struct clap_event_header>
#field param_id , CUInt
#field cookie , Ptr ()
#field note_id , CInt
#field port_index , CShort
#field channel , CShort
#field key , CShort
#field value , CDouble
#stoptype
#synonym_t clap_event_param_value_t , <struct clap_event_param_value>
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
#starttype struct clap_event_param_mod
#field header , <struct clap_event_header>
#field param_id , CUInt
#field cookie , Ptr ()
#field note_id , CInt
#field port_index , CShort
#field channel , CShort
#field key , CShort
#field amount , CDouble
#stoptype
#synonym_t clap_event_param_mod_t , <struct clap_event_param_mod>
{- typedef struct clap_event_param_gesture {
            clap_event_header_t header; clap_id param_id;
        } clap_event_param_gesture_t; -}
#starttype struct clap_event_param_gesture
#field header , <struct clap_event_header>
#field param_id , CUInt
#stoptype
#synonym_t clap_event_param_gesture_t , <struct clap_event_param_gesture>
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
#integral_t enum clap_transport_flags
#num CLAP_TRANSPORT_HAS_TEMPO
#num CLAP_TRANSPORT_HAS_BEATS_TIMELINE
#num CLAP_TRANSPORT_HAS_SECONDS_TIMELINE
#num CLAP_TRANSPORT_HAS_TIME_SIGNATURE
#num CLAP_TRANSPORT_IS_PLAYING
#num CLAP_TRANSPORT_IS_RECORDING
#num CLAP_TRANSPORT_IS_LOOP_ACTIVE
#num CLAP_TRANSPORT_IS_WITHIN_PRE_ROLL
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
#starttype struct clap_event_transport
#field header , <struct clap_event_header>
#field flags , CUInt
#field song_pos_beats , CLong
#field song_pos_seconds , CLong
#field tempo , CDouble
#field tempo_inc , CDouble
#field loop_start_beats , CLong
#field loop_end_beats , CLong
#field loop_start_seconds , CLong
#field loop_end_seconds , CLong
#field bar_start , CLong
#field bar_number , CInt
#field tsig_num , CUShort
#field tsig_denom , CUShort
#stoptype
#synonym_t clap_event_transport_t , <struct clap_event_transport>
{- typedef struct clap_event_midi {
            clap_event_header_t header; uint16_t port_index; uint8_t data[3];
        } clap_event_midi_t; -}
#starttype struct clap_event_midi
#field header , <struct clap_event_header>
#field port_index , CUShort
#array_field data , CUChar
#stoptype
#synonym_t clap_event_midi_t , <struct clap_event_midi>
{- typedef struct clap_event_midi_sysex {
            clap_event_header_t header;
            uint16_t port_index;
            const uint8_t * buffer;
            uint32_t size;
        } clap_event_midi_sysex_t; -}
#starttype struct clap_event_midi_sysex
#field header , <struct clap_event_header>
#field port_index , CUShort
#field buffer , Ptr CUChar
#field size , CUInt
#stoptype
#synonym_t clap_event_midi_sysex_t , <struct clap_event_midi_sysex>
{- typedef struct clap_event_midi2 {
            clap_event_header_t header; uint16_t port_index; uint32_t data[4];
        } clap_event_midi2_t; -}
#starttype struct clap_event_midi2
#field header , <struct clap_event_header>
#field port_index , CUShort
#array_field data , CUInt
#stoptype
#synonym_t clap_event_midi2_t , <struct clap_event_midi2>
{- typedef struct clap_input_events {
            void * ctx;
            uint32_t (* size)(const struct clap_input_events * list);
            const clap_event_header_t * (* get)(const struct clap_input_events * list,
                                                uint32_t index);
        } clap_input_events_t; -}
#starttype struct clap_input_events
#field ctx , Ptr ()
#field size , FunPtr (Ptr <struct clap_input_events> -> IO CUInt)
#field get , FunPtr (Ptr <struct clap_input_events> -> CUInt -> IO (Ptr <struct clap_event_header>))
#stoptype
#synonym_t clap_input_events_t , <struct clap_input_events>
#callback_t size , Ptr <struct clap_input_events> -> IO CUInt
#callback_t get , Ptr <struct clap_input_events> -> CUInt -> IO (Ptr <struct clap_event_header>)
{- typedef struct clap_output_events {
            void * ctx;
            _Bool (* try_push)(const struct clap_output_events * list,
                               const clap_event_header_t * event);
        } clap_output_events_t; -}
#starttype struct clap_output_events
#field ctx , Ptr ()
#field try_push , FunPtr (Ptr <struct clap_output_events> -> Ptr <struct clap_event_header> -> IO CInt)
#stoptype
#synonym_t clap_output_events_t , <struct clap_output_events>
#callback_t try_push , Ptr <struct clap_output_events> -> Ptr <struct clap_event_header> -> IO CInt
