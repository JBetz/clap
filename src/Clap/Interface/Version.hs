module Clap.Interface.Version where

import Clap.Interface.Foreign
import Clap.Interface.Foreign.Version
import Data.Int
import Data.Word
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

data ClapVersion = ClapVersion
    { clapVersion_major :: Word32
    , clapVersion_minor :: Word32
    , clapVersion_revision :: Word32
    } deriving (Eq, Show)

hostClapVersion :: ClapVersion
hostClapVersion = ClapVersion
    { clapVersion_major = fromIntegral clapVersionMajor
    , clapVersion_minor = fromIntegral clapVersionMinor
    , clapVersion_revision = fromIntegral clapVersionRevision 
    }

clapVersionIsCompatible :: ClapVersion -> Bool
clapVersionIsCompatible (ClapVersion major minor revision) = major >= 1

fromStruct :: C'clap_version -> ClapVersion
fromStruct (C'clap_version major minor revision) = ClapVersion
    { clapVersion_major = fromIntegral major 
    , clapVersion_minor = fromIntegral minor
    , clapVersion_revision = fromIntegral revision
    }

toStruct :: ClapVersion -> C'clap_version
toStruct (ClapVersion major minor revision) = C'clap_version
    { c'clap_version'major = fromIntegral major 
    , c'clap_version'minor = fromIntegral minor
    , c'clap_version'revision = fromIntegral revision
    }