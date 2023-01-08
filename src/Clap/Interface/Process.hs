module Clap.Interface.Process where

import Clap.Interface.Foreign.Process
import Foreign.Ptr

newtype ProcessHandle = ProcessHandle { unProcessHandle :: Ptr C'clap_process }

data ProcessStatus 
    = Error
    | Continue
    | ContinueIfNotQuiet
    | Tail 
    | Sleep
    deriving (Enum)