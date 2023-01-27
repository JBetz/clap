{-# LINE 1 "src/Clap/Interface/Foreign/Host.hsc" #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}


module Clap.Interface.Foreign.Host where
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

{-# LINE 7 "src/Clap/Interface/Foreign/Host.hsc" #-}

import Clap.Interface.Foreign.Version
{- typedef struct clap_host {
            clap_version_t clap_version;
            void * host_data;
            const char * name;
            const char * vendor;
            const char * url;
            const char * version;
            const void * (* get_extension)(const struct clap_host * host,
                                           const char * extension_id);
            void (* request_restart)(const struct clap_host * host);
            void (* request_process)(const struct clap_host * host);
            void (* request_callback)(const struct clap_host * host);
        } clap_host_t; -}

{-# LINE 23 "src/Clap/Interface/Foreign/Host.hsc" #-}

{-# LINE 24 "src/Clap/Interface/Foreign/Host.hsc" #-}

{-# LINE 25 "src/Clap/Interface/Foreign/Host.hsc" #-}

{-# LINE 26 "src/Clap/Interface/Foreign/Host.hsc" #-}

{-# LINE 27 "src/Clap/Interface/Foreign/Host.hsc" #-}

{-# LINE 28 "src/Clap/Interface/Foreign/Host.hsc" #-}

{-# LINE 29 "src/Clap/Interface/Foreign/Host.hsc" #-}

{-# LINE 30 "src/Clap/Interface/Foreign/Host.hsc" #-}

{-# LINE 31 "src/Clap/Interface/Foreign/Host.hsc" #-}

{-# LINE 32 "src/Clap/Interface/Foreign/Host.hsc" #-}

{-# LINE 33 "src/Clap/Interface/Foreign/Host.hsc" #-}
data C'clap_host = C'clap_host{
  c'clap_host'clap_version :: C'clap_version,
  c'clap_host'host_data :: Ptr (),
  c'clap_host'name :: CString,
  c'clap_host'vendor :: CString,
  c'clap_host'url :: CString,
  c'clap_host'version :: CString,
  c'clap_host'get_extension :: FunPtr (Ptr C'clap_host -> CString -> IO (Ptr ())),
  c'clap_host'request_restart :: FunPtr (Ptr C'clap_host -> IO ()),
  c'clap_host'request_process :: FunPtr (Ptr C'clap_host -> IO ()),
  c'clap_host'request_callback :: FunPtr (Ptr C'clap_host -> IO ())
} deriving (Eq,Show)
p'clap_host'clap_version p = plusPtr p 0
p'clap_host'clap_version :: Ptr (C'clap_host) -> Ptr (C'clap_version)
p'clap_host'host_data p = plusPtr p 16
p'clap_host'host_data :: Ptr (C'clap_host) -> Ptr (Ptr ())
p'clap_host'name p = plusPtr p 24
p'clap_host'name :: Ptr (C'clap_host) -> Ptr (CString)
p'clap_host'vendor p = plusPtr p 32
p'clap_host'vendor :: Ptr (C'clap_host) -> Ptr (CString)
p'clap_host'url p = plusPtr p 40
p'clap_host'url :: Ptr (C'clap_host) -> Ptr (CString)
p'clap_host'version p = plusPtr p 48
p'clap_host'version :: Ptr (C'clap_host) -> Ptr (CString)
p'clap_host'get_extension p = plusPtr p 56
p'clap_host'get_extension :: Ptr (C'clap_host) -> Ptr (FunPtr (Ptr C'clap_host -> CString -> IO (Ptr ())))
p'clap_host'request_restart p = plusPtr p 64
p'clap_host'request_restart :: Ptr (C'clap_host) -> Ptr (FunPtr (Ptr C'clap_host -> IO ()))
p'clap_host'request_process p = plusPtr p 72
p'clap_host'request_process :: Ptr (C'clap_host) -> Ptr (FunPtr (Ptr C'clap_host -> IO ()))
p'clap_host'request_callback p = plusPtr p 80
p'clap_host'request_callback :: Ptr (C'clap_host) -> Ptr (FunPtr (Ptr C'clap_host -> IO ()))
instance Storable C'clap_host where
  sizeOf _ = 88
  alignment _ = 8
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 16
    v2 <- peekByteOff _p 24
    v3 <- peekByteOff _p 32
    v4 <- peekByteOff _p 40
    v5 <- peekByteOff _p 48
    v6 <- peekByteOff _p 56
    v7 <- peekByteOff _p 64
    v8 <- peekByteOff _p 72
    v9 <- peekByteOff _p 80
    return $ C'clap_host v0 v1 v2 v3 v4 v5 v6 v7 v8 v9
  poke _p (C'clap_host v0 v1 v2 v3 v4 v5 v6 v7 v8 v9) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 16 v1
    pokeByteOff _p 24 v2
    pokeByteOff _p 32 v3
    pokeByteOff _p 40 v4
    pokeByteOff _p 48 v5
    pokeByteOff _p 56 v6
    pokeByteOff _p 64 v7
    pokeByteOff _p 72 v8
    pokeByteOff _p 80 v9
    return ()

{-# LINE 34 "src/Clap/Interface/Foreign/Host.hsc" #-}
type C'clap_host_t = C'clap_host

{-# LINE 35 "src/Clap/Interface/Foreign/Host.hsc" #-}
type C'get_extension = FunPtr (Ptr C'clap_host -> CString -> IO (Ptr ()))
foreign import ccall "wrapper" mk'get_extension
  :: (Ptr C'clap_host -> CString -> IO (Ptr ())) -> IO C'get_extension
foreign import ccall "dynamic" mK'get_extension
  :: C'get_extension -> (Ptr C'clap_host -> CString -> IO (Ptr ()))

{-# LINE 36 "src/Clap/Interface/Foreign/Host.hsc" #-}
type C'request_restart = FunPtr (Ptr C'clap_host -> IO ())
foreign import ccall "wrapper" mk'request_restart
  :: (Ptr C'clap_host -> IO ()) -> IO C'request_restart
foreign import ccall "dynamic" mK'request_restart
  :: C'request_restart -> (Ptr C'clap_host -> IO ())

{-# LINE 37 "src/Clap/Interface/Foreign/Host.hsc" #-}
type C'request_process = FunPtr (Ptr C'clap_host -> IO ())
foreign import ccall "wrapper" mk'request_process
  :: (Ptr C'clap_host -> IO ()) -> IO C'request_process
foreign import ccall "dynamic" mK'request_process
  :: C'request_process -> (Ptr C'clap_host -> IO ())

{-# LINE 38 "src/Clap/Interface/Foreign/Host.hsc" #-}
type C'request_callback = FunPtr (Ptr C'clap_host -> IO ())
foreign import ccall "wrapper" mk'request_callback
  :: (Ptr C'clap_host -> IO ()) -> IO C'request_callback
foreign import ccall "dynamic" mK'request_callback
  :: C'request_callback -> (Ptr C'clap_host -> IO ())
