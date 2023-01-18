module Clap.Interface.Process where

import Clap.Interface.AudioBuffer
import Clap.Interface.Events
import Clap.Interface.Foreign.Events
import Clap.Interface.Foreign.Process
import Data.Int
import Data.Word
import Foreign.Marshal.Utils
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

createProcess :: IO ProcessHandle 
createProcess = new $ C'clap_process 
    { c'clap_process'steady_time = 0
    , c'clap_process'frames_count = 0
    , c'clap_process'transport = nullPtr
    , c'clap_process'audio_inputs = nullPtr
    , c'clap_process'audio_outputs = nullPtr
    , c'clap_process'audio_inputs_count = 0
    , c'clap_process'audio_outputs_count = 0 
    , c'clap_process'in_events = nullPtr
    , c'clap_process'out_events = nullPtr
    }
 
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


