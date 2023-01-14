module Clap.Interface.Process where

import Clap.Interface.Foreign.Process
import Foreign.Ptr

type ProcessHandle = Ptr C'clap_process

data ProcessStatus 
    = Error
    | Continue
    | ContinueIfNotQuiet
    | Tail 
    | Sleep
    deriving (Enum)