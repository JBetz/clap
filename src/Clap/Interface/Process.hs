module Clap.Interface.Process where

import Clap.Interface.AudioBuffer
import Clap.Interface.Events
import Clap.Interface.Foreign.Events
import Clap.Interface.Foreign.Process
import Data.Int
import Data.Word
import Foreign.Ptr
import Foreign.Storable

type ProcessHandle = Ptr C'clap_process

data ProcessStatus 
    = Error
    | Continue
    | ContinueIfNotQuiet
    | Tail 
    | Sleep
    deriving (Enum)

setSteadyTime :: ProcessHandle -> Int64 -> IO ()
setSteadyTime process =
    poke (p'clap_process'steady_time process) . fromIntegral

setFramesCount :: ProcessHandle -> Word64 -> IO ()
setFramesCount process =
    poke (p'clap_process'frames_count process) . fromIntegral

setTransport :: ProcessHandle -> Ptr C'clap_event_transport -> IO ()
setTransport process =
    poke (p'clap_process'transport process)

setInputEvents :: ProcessHandle -> InputEventsHandle -> IO ()
setInputEvents process =
    poke (p'clap_process'in_events process)

setOutputEvents :: ProcessHandle -> OutputEventsHandle -> IO ()
setOutputEvents process =
    poke (p'clap_process'out_events process)

setAudioInputs :: ProcessHandle -> AudioBufferHandle -> IO ()
setAudioInputs process =
    poke (p'clap_process'audio_inputs process)

setAudioOutputs :: ProcessHandle -> AudioBufferHandle -> IO ()
setAudioOutputs process =
    poke (p'clap_process'audio_outputs process)

setAudioInputsCount :: ProcessHandle -> Word32 -> IO ()
setAudioInputsCount process =
    poke (p'clap_process'audio_inputs_count process) . fromIntegral 

setAudioOutputsCount :: ProcessHandle -> Word32 -> IO ()
setAudioOutputsCount process =
    poke (p'clap_process'audio_outputs_count process) . fromIntegral


