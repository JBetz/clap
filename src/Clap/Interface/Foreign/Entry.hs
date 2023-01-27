{-# LINE 1 "src/Clap/Interface/Foreign/Entry.hsc" #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}




module Clap.Interface.Foreign.Entry where

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

{-# LINE 10 "src/Clap/Interface/Foreign/Entry.hsc" #-}

import Clap.Interface.Foreign.Version
{- typedef struct clap_plugin_entry {
            clap_version_t clap_version;
            _Bool (* init)(const char * plugin_path);
            void (* deinit)(void);
            const void * (* get_factory)(const char * factory_id);
        } clap_plugin_entry_t; -}

{-# LINE 19 "src/Clap/Interface/Foreign/Entry.hsc" #-}

{-# LINE 20 "src/Clap/Interface/Foreign/Entry.hsc" #-}

{-# LINE 21 "src/Clap/Interface/Foreign/Entry.hsc" #-}

{-# LINE 22 "src/Clap/Interface/Foreign/Entry.hsc" #-}

{-# LINE 23 "src/Clap/Interface/Foreign/Entry.hsc" #-}
data C'clap_plugin_entry = C'clap_plugin_entry{
  c'clap_plugin_entry'clap_version :: C'clap_version,
  c'clap_plugin_entry'init :: FunPtr (CString -> CBool),
  c'clap_plugin_entry'deinit :: FunPtr (IO ()),
  c'clap_plugin_entry'get_factory :: FunPtr (CString -> Ptr ())
} deriving (Eq,Show)
p'clap_plugin_entry'clap_version p = plusPtr p 0
p'clap_plugin_entry'clap_version :: Ptr (C'clap_plugin_entry) -> Ptr (C'clap_version)
p'clap_plugin_entry'init p = plusPtr p 16
p'clap_plugin_entry'init :: Ptr (C'clap_plugin_entry) -> Ptr (FunPtr (CString -> CBool))
p'clap_plugin_entry'deinit p = plusPtr p 24
p'clap_plugin_entry'deinit :: Ptr (C'clap_plugin_entry) -> Ptr (FunPtr (IO ()))
p'clap_plugin_entry'get_factory p = plusPtr p 32
p'clap_plugin_entry'get_factory :: Ptr (C'clap_plugin_entry) -> Ptr (FunPtr (CString -> Ptr ()))
instance Storable C'clap_plugin_entry where
  sizeOf _ = 40
  alignment _ = 8
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 16
    v2 <- peekByteOff _p 24
    v3 <- peekByteOff _p 32
    return $ C'clap_plugin_entry v0 v1 v2 v3
  poke _p (C'clap_plugin_entry v0 v1 v2 v3) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 16 v1
    pokeByteOff _p 24 v2
    pokeByteOff _p 32 v3
    return ()

{-# LINE 24 "src/Clap/Interface/Foreign/Entry.hsc" #-}
type C'clap_plugin_entry_t = C'clap_plugin_entry

{-# LINE 25 "src/Clap/Interface/Foreign/Entry.hsc" #-}
type C'init = FunPtr (CString -> CBool)
foreign import ccall "wrapper" mk'init
  :: (CString -> CBool) -> IO C'init
foreign import ccall "dynamic" mK'init
  :: C'init -> (CString -> CBool)

{-# LINE 26 "src/Clap/Interface/Foreign/Entry.hsc" #-}
type C'deinit = FunPtr (IO ())
foreign import ccall "wrapper" mk'deinit
  :: (IO ()) -> IO C'deinit
foreign import ccall "dynamic" mK'deinit
  :: C'deinit -> (IO ())

{-# LINE 27 "src/Clap/Interface/Foreign/Entry.hsc" #-}
type C'get_factory = FunPtr (CString -> Ptr ())
foreign import ccall "wrapper" mk'get_factory
  :: (CString -> Ptr ()) -> IO C'get_factory
foreign import ccall "dynamic" mK'get_factory
  :: C'get_factory -> (CString -> Ptr ())
