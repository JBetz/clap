{-# LINE 1 "src/Clap/Interface/Foreign/Color.hsc" #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}


module Clap.Interface.Foreign.Color where
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

{-# LINE 7 "src/Clap/Interface/Foreign/Color.hsc" #-}

{- typedef struct clap_color {
            uint8_t alpha; uint8_t red; uint8_t green; uint8_t blue;
        } clap_color_t; -}

{-# LINE 12 "src/Clap/Interface/Foreign/Color.hsc" #-}

{-# LINE 13 "src/Clap/Interface/Foreign/Color.hsc" #-}

{-# LINE 14 "src/Clap/Interface/Foreign/Color.hsc" #-}

{-# LINE 15 "src/Clap/Interface/Foreign/Color.hsc" #-}

{-# LINE 16 "src/Clap/Interface/Foreign/Color.hsc" #-}
data C'clap_color = C'clap_color{
  c'clap_color'alpha :: CUChar,
  c'clap_color'red :: CUChar,
  c'clap_color'green :: CUChar,
  c'clap_color'blue :: CUChar
} deriving (Eq,Show)
p'clap_color'alpha p = plusPtr p 0
p'clap_color'alpha :: Ptr (C'clap_color) -> Ptr (CUChar)
p'clap_color'red p = plusPtr p 1
p'clap_color'red :: Ptr (C'clap_color) -> Ptr (CUChar)
p'clap_color'green p = plusPtr p 2
p'clap_color'green :: Ptr (C'clap_color) -> Ptr (CUChar)
p'clap_color'blue p = plusPtr p 3
p'clap_color'blue :: Ptr (C'clap_color) -> Ptr (CUChar)
instance Storable C'clap_color where
  sizeOf _ = 4
  alignment _ = 1
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 1
    v2 <- peekByteOff _p 2
    v3 <- peekByteOff _p 3
    return $ C'clap_color v0 v1 v2 v3
  poke _p (C'clap_color v0 v1 v2 v3) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 1 v1
    pokeByteOff _p 2 v2
    pokeByteOff _p 3 v3
    return ()

{-# LINE 17 "src/Clap/Interface/Foreign/Color.hsc" #-}
type C'clap_color_t = C'clap_color

{-# LINE 18 "src/Clap/Interface/Foreign/Color.hsc" #-}
