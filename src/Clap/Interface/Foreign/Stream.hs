{-# LINE 1 "src/Clap/Interface/Foreign/Stream.hsc" #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}


module Clap.Interface.Foreign.Stream where
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

{-# LINE 7 "src/Clap/Interface/Foreign/Stream.hsc" #-}

{- typedef struct clap_istream {
            void * ctx;
            int64_t (* read)(const struct clap_istream * stream,
                             void * buffer,
                             uint64_t size);
        } clap_istream_t; -}

{-# LINE 15 "src/Clap/Interface/Foreign/Stream.hsc" #-}

{-# LINE 16 "src/Clap/Interface/Foreign/Stream.hsc" #-}

{-# LINE 17 "src/Clap/Interface/Foreign/Stream.hsc" #-}
data C'clap_istream = C'clap_istream{
  c'clap_istream'ctx :: Ptr (),
  c'clap_istream'read :: FunPtr (Ptr C'clap_istream -> Ptr () -> CULong -> CLong)
} deriving (Eq,Show)
p'clap_istream'ctx p = plusPtr p 0
p'clap_istream'ctx :: Ptr (C'clap_istream) -> Ptr (Ptr ())
p'clap_istream'read p = plusPtr p 8
p'clap_istream'read :: Ptr (C'clap_istream) -> Ptr (FunPtr (Ptr C'clap_istream -> Ptr () -> CULong -> CLong))
instance Storable C'clap_istream where
  sizeOf _ = 16
  alignment _ = 8
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 8
    return $ C'clap_istream v0 v1
  poke _p (C'clap_istream v0 v1) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 8 v1
    return ()

{-# LINE 18 "src/Clap/Interface/Foreign/Stream.hsc" #-}
type C'clap_istream_t = C'clap_istream

{-# LINE 19 "src/Clap/Interface/Foreign/Stream.hsc" #-}
type C'read = FunPtr (Ptr C'clap_istream -> Ptr () -> CULong -> CLong)
foreign import ccall "wrapper" mk'read
  :: (Ptr C'clap_istream -> Ptr () -> CULong -> CLong) -> IO C'read
foreign import ccall "dynamic" mK'read
  :: C'read -> (Ptr C'clap_istream -> Ptr () -> CULong -> CLong)

{-# LINE 20 "src/Clap/Interface/Foreign/Stream.hsc" #-}
{- typedef struct clap_ostream {
            void * ctx;
            int64_t (* write)(const struct clap_ostream * stream,
                              const void * buffer,
                              uint64_t size);
        } clap_ostream_t; -}

{-# LINE 27 "src/Clap/Interface/Foreign/Stream.hsc" #-}

{-# LINE 28 "src/Clap/Interface/Foreign/Stream.hsc" #-}

{-# LINE 29 "src/Clap/Interface/Foreign/Stream.hsc" #-}
data C'clap_ostream = C'clap_ostream{
  c'clap_ostream'ctx :: Ptr (),
  c'clap_ostream'write :: FunPtr (Ptr C'clap_ostream -> Ptr () -> CULong -> CLong)
} deriving (Eq,Show)
p'clap_ostream'ctx p = plusPtr p 0
p'clap_ostream'ctx :: Ptr (C'clap_ostream) -> Ptr (Ptr ())
p'clap_ostream'write p = plusPtr p 8
p'clap_ostream'write :: Ptr (C'clap_ostream) -> Ptr (FunPtr (Ptr C'clap_ostream -> Ptr () -> CULong -> CLong))
instance Storable C'clap_ostream where
  sizeOf _ = 16
  alignment _ = 8
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 8
    return $ C'clap_ostream v0 v1
  poke _p (C'clap_ostream v0 v1) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 8 v1
    return ()

{-# LINE 30 "src/Clap/Interface/Foreign/Stream.hsc" #-}
type C'clap_ostream_t = C'clap_ostream

{-# LINE 31 "src/Clap/Interface/Foreign/Stream.hsc" #-}
type C'write = FunPtr (Ptr C'clap_ostream -> Ptr () -> CULong -> CLong)
foreign import ccall "wrapper" mk'write
  :: (Ptr C'clap_ostream -> Ptr () -> CULong -> CLong) -> IO C'write
foreign import ccall "dynamic" mK'write
  :: C'write -> (Ptr C'clap_ostream -> Ptr () -> CULong -> CLong)

{-# LINE 32 "src/Clap/Interface/Foreign/Stream.hsc" #-}
