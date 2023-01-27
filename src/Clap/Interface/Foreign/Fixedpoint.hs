{-# LINE 1 "src/Clap/Interface/Foreign/Fixedpoint.hsc" #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}


module Clap.Interface.Foreign.Fixedpoint where
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

{-# LINE 7 "src/Clap/Interface/Foreign/Fixedpoint.hsc" #-}

{- typedef int64_t clap_beattime; -}
type C'clap_beattime = CLong

{-# LINE 10 "src/Clap/Interface/Foreign/Fixedpoint.hsc" #-}
{- typedef int64_t clap_sectime; -}
type C'clap_sectime = CLong

{-# LINE 12 "src/Clap/Interface/Foreign/Fixedpoint.hsc" #-}
