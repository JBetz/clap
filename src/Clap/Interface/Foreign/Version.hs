{-# LINE 1 "src/Clap/Interface/Foreign/Version.hsc" #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE CApiFFI #-}


module Clap.Interface.Foreign.Version where
import Foreign.Ptr
import Foreign.Ptr (Ptr,FunPtr,plusPtr)
import Foreign.Ptr (wordPtrToPtr,castPtrToFunPtr)
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String (CString,CStringLen,CWString,CWStringLen)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (peekArray,pokeArray)
import Data.Int
import Data.Word

{-# LINE 8 "src/Clap/Interface/Foreign/Version.hsc" #-}

{- typedef struct clap_version {
            uint32_t major; uint32_t minor; uint32_t revision;
        } clap_version_t; -}

{-# LINE 13 "src/Clap/Interface/Foreign/Version.hsc" #-}

{-# LINE 14 "src/Clap/Interface/Foreign/Version.hsc" #-}

{-# LINE 15 "src/Clap/Interface/Foreign/Version.hsc" #-}

{-# LINE 16 "src/Clap/Interface/Foreign/Version.hsc" #-}
data C'clap_version = C'clap_version{
  c'clap_version'major :: CUInt,
  c'clap_version'minor :: CUInt,
  c'clap_version'revision :: CUInt
} deriving (Eq,Show)
p'clap_version'major p = plusPtr p 0
p'clap_version'major :: Ptr (C'clap_version) -> Ptr (CUInt)
p'clap_version'minor p = plusPtr p 4
p'clap_version'minor :: Ptr (C'clap_version) -> Ptr (CUInt)
p'clap_version'revision p = plusPtr p 8
p'clap_version'revision :: Ptr (C'clap_version) -> Ptr (CUInt)
instance Storable C'clap_version where
  sizeOf _ = 12
  alignment _ = 4
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 4
    v2 <- peekByteOff _p 8
    return $ C'clap_version v0 v1 v2
  poke _p (C'clap_version v0 v1 v2) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 4 v1
    pokeByteOff _p 8 v2
    return ()

{-# LINE 17 "src/Clap/Interface/Foreign/Version.hsc" #-}
type C'clap_version_t = C'clap_version

{-# LINE 18 "src/Clap/Interface/Foreign/Version.hsc" #-}
clapVersionMajor, clapVersionMinor, clapVersionRevision :: CUInt
clapVersionMajor = 1
{-# LINE 20 "src/Clap/Interface/Foreign/Version.hsc" #-}
clapVersionMinor = 1
{-# LINE 21 "src/Clap/Interface/Foreign/Version.hsc" #-}
clapVersionRevision = 6