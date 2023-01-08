module Clap.Interface.Version where

import Clap.Interface.Foreign
import Clap.Interface.Foreign.Version
import Data.Int
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import System.Posix

data ClapVersion = ClapVersion
    { clapVersion_major :: Int
    , clapVersion_minor :: Int
    , clapVersion_revision :: Int 
    } deriving (Eq, Show)

clapVersion :: ClapVersion
clapVersion = ClapVersion
    { clapVersion_major = fromIntegral clapVersionMajor
    , clapVersion_minor = fromIntegral clapVersionMinor
    , clapVersion_revision = fromIntegral clapVersionRevision 
    }

clapVersionIsCompatible :: ClapVersion -> Bool
clapVersionIsCompatible (ClapVersion major minor revision) = major >= 1

fromStruct :: C'clap_version -> ClapVersion
fromStruct (C'clap_version major minor revision) = ClapVersion
    { clapVersion_major = fromCUInt major 
    , clapVersion_minor = fromCUInt minor
    , clapVersion_revision = fromCUInt revision
    }

toStruct :: ClapVersion -> C'clap_version
toStruct (ClapVersion major minor revision) = C'clap_version
    { c'clap_version'major = toCUInt major 
    , c'clap_version'minor = toCUInt minor
    , c'clap_version'revision = toCUInt revision
    }