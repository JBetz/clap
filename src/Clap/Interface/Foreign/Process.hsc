{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <process.h>
module Clap.Interface.Foreign.Process where
import Foreign.Ptr
#strict_import

import Clap.Interface.Foreign.Events
import Clap.Interface.Foreign.AudioBuffer

{- typedef int32_t clap_process_status; -}
#synonym_t clap_process_status , CInt
{- typedef struct clap_process {
            int64_t steady_time;
            uint32_t frames_count;
            const clap_event_transport_t * transport;
            const clap_audio_buffer_t * audio_inputs;
            clap_audio_buffer_t * audio_outputs;
            uint32_t audio_inputs_count;
            uint32_t audio_outputs_count;
            const clap_input_events_t * in_events;
            const clap_output_events_t * out_events;
        } clap_process_t; -}
#starttype struct clap_process
#field steady_time , CLong
#field frames_count , CUInt
#field transport , Ptr <struct clap_event_transport>
#field audio_inputs , Ptr <struct clap_audio_buffer>
#field audio_outputs , Ptr <struct clap_audio_buffer>
#field audio_inputs_count , CUInt
#field audio_outputs_count , CUInt
#field in_events , Ptr <struct clap_input_events>
#field out_events , Ptr <struct clap_output_events>
#stoptype
#synonym_t clap_process_t , <struct clap_process>
