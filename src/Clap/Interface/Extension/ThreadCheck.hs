module Clap.Interface.Extension.ThreadCheck where

import Clap.Interface.Extension.Foreign.ThreadCheck
import Clap.Interface.Host
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

type HostThreadCheckHandle = Ptr C'clap_host_thread_check
    
isMainThread :: HostThreadCheckHandle -> HostHandle -> IO Bool
isMainThread hostThreadCheck (HostHandle host) = do
    funPtr <- peek $ p'clap_host_thread_check'is_main_thread hostThreadCheck
    pure $ toBool $ mK'is_main_thread funPtr host

isAudioThread :: HostThreadCheckHandle -> HostHandle -> IO Bool
isAudioThread hostThreadCheck (HostHandle host) = do
    funPtr <- peek $ p'clap_host_thread_check'is_audio_thread hostThreadCheck
    pure $ toBool $ mK'is_audio_thread funPtr host
