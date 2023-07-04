{-# LINE 1 "src/Clap/Interface/Extension/Foreign/Gui.hsc" #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}



module Clap.Interface.Extension.Foreign.Gui where
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

{-# LINE 8 "src/Clap/Interface/Extension/Foreign/Gui.hsc" #-}

import Clap.Interface.Foreign.Host
import Clap.Interface.Foreign.Plugin (C'clap_plugin)
-- #globalarray CLAP_EXT_GUI , CChar
foreign import ccall "array_CLAP_WINDOW_API_WIN32" c'CLAP_WINDOW_API_WIN32
  :: Ptr (CChar)

{-# LINE 13 "src/Clap/Interface/Extension/Foreign/Gui.hsc" #-}
foreign import ccall "array_CLAP_WINDOW_API_COCOA" c'CLAP_WINDOW_API_COCOA
  :: Ptr (CChar)

{-# LINE 14 "src/Clap/Interface/Extension/Foreign/Gui.hsc" #-}
foreign import ccall "array_CLAP_WINDOW_API_X11" c'CLAP_WINDOW_API_X11
  :: Ptr (CChar)

{-# LINE 15 "src/Clap/Interface/Extension/Foreign/Gui.hsc" #-}
foreign import ccall "array_CLAP_WINDOW_API_WAYLAND" c'CLAP_WINDOW_API_WAYLAND
  :: Ptr (CChar)

{-# LINE 16 "src/Clap/Interface/Extension/Foreign/Gui.hsc" #-}
-- #globalvar clap_hwnd , Ptr ()
-- #globalvar clap_nsview , Ptr ()
{- typedef unsigned long clap_xwnd; -}
type C'clap_xwnd = CULong

{-# LINE 20 "src/Clap/Interface/Extension/Foreign/Gui.hsc" #-}
{- typedef struct clap_window {
            const char * api;
            union {
                clap_nsview cocoa; clap_xwnd x11; clap_hwnd win32; void * ptr;
            };
        } clap_window_t; -}

{-# LINE 27 "src/Clap/Interface/Extension/Foreign/Gui.hsc" #-}

{-# LINE 28 "src/Clap/Interface/Extension/Foreign/Gui.hsc" #-}
-- #field handle , Ptr ()
data C'clap_window = C'clap_window{
  c'clap_window'api :: CString,
  c'clap_window'handle :: Ptr ()
} deriving (Eq,Show)
p'clap_window'api p = plusPtr p 0
p'clap_window'api :: Ptr (C'clap_window) -> Ptr (CString)
instance Storable C'clap_window where
  sizeOf _ = 16
  alignment _ = 8
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 8
    return $ C'clap_window v0 v1
  poke _p (C'clap_window v0 v1) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 8 v1
    return ()

{-# LINE 30 "src/Clap/Interface/Extension/Foreign/Gui.hsc" #-}
type C'clap_window_t = C'clap_window

{-# LINE 31 "src/Clap/Interface/Extension/Foreign/Gui.hsc" #-}
{- typedef struct clap_gui_resize_hints {
            _Bool can_resize_horizontally;
            _Bool can_resize_vertically;
            _Bool preserve_aspect_ratio;
            uint32_t aspect_ratio_width;
            uint32_t aspect_ratio_height;
        } clap_gui_resize_hints_t; -}

{-# LINE 39 "src/Clap/Interface/Extension/Foreign/Gui.hsc" #-}

{-# LINE 40 "src/Clap/Interface/Extension/Foreign/Gui.hsc" #-}

{-# LINE 41 "src/Clap/Interface/Extension/Foreign/Gui.hsc" #-}

{-# LINE 42 "src/Clap/Interface/Extension/Foreign/Gui.hsc" #-}

{-# LINE 43 "src/Clap/Interface/Extension/Foreign/Gui.hsc" #-}

{-# LINE 44 "src/Clap/Interface/Extension/Foreign/Gui.hsc" #-}
data C'clap_gui_resize_hints = C'clap_gui_resize_hints{
  c'clap_gui_resize_hints'can_resize_horizontally :: CInt,
  c'clap_gui_resize_hints'can_resize_vertically :: CInt,
  c'clap_gui_resize_hints'preserve_aspect_ratio :: CInt,
  c'clap_gui_resize_hints'aspect_ratio_width :: CUInt,
  c'clap_gui_resize_hints'aspect_ratio_height :: CUInt
} deriving (Eq,Show)
p'clap_gui_resize_hints'can_resize_horizontally p = plusPtr p 0
p'clap_gui_resize_hints'can_resize_horizontally :: Ptr (C'clap_gui_resize_hints) -> Ptr (CInt)
p'clap_gui_resize_hints'can_resize_vertically p = plusPtr p 1
p'clap_gui_resize_hints'can_resize_vertically :: Ptr (C'clap_gui_resize_hints) -> Ptr (CInt)
p'clap_gui_resize_hints'preserve_aspect_ratio p = plusPtr p 2
p'clap_gui_resize_hints'preserve_aspect_ratio :: Ptr (C'clap_gui_resize_hints) -> Ptr (CInt)
p'clap_gui_resize_hints'aspect_ratio_width p = plusPtr p 4
p'clap_gui_resize_hints'aspect_ratio_width :: Ptr (C'clap_gui_resize_hints) -> Ptr (CUInt)
p'clap_gui_resize_hints'aspect_ratio_height p = plusPtr p 8
p'clap_gui_resize_hints'aspect_ratio_height :: Ptr (C'clap_gui_resize_hints) -> Ptr (CUInt)
instance Storable C'clap_gui_resize_hints where
  sizeOf _ = 12
  alignment _ = 4
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 1
    v2 <- peekByteOff _p 2
    v3 <- peekByteOff _p 4
    v4 <- peekByteOff _p 8
    return $ C'clap_gui_resize_hints v0 v1 v2 v3 v4
  poke _p (C'clap_gui_resize_hints v0 v1 v2 v3 v4) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 1 v1
    pokeByteOff _p 2 v2
    pokeByteOff _p 4 v3
    pokeByteOff _p 8 v4
    return ()

{-# LINE 45 "src/Clap/Interface/Extension/Foreign/Gui.hsc" #-}
type C'clap_gui_resize_hints_t = C'clap_gui_resize_hints

{-# LINE 46 "src/Clap/Interface/Extension/Foreign/Gui.hsc" #-}
{- typedef struct clap_plugin_gui {
            _Bool (* is_api_supported)(const clap_plugin_t * plugin,
                                       const char * api,
                                       _Bool is_floating);
            _Bool (* get_preferred_api)(const clap_plugin_t * plugin,
                                        const char * * api,
                                        _Bool * is_floating);
            _Bool (* create)(const clap_plugin_t * plugin,
                             const char * api,
                             _Bool is_floating);
            void (* destroy)(const clap_plugin_t * plugin);
            _Bool (* set_scale)(const clap_plugin_t * plugin, double scale);
            _Bool (* get_size)(const clap_plugin_t * plugin,
                               uint32_t * width,
                               uint32_t * height);
            _Bool (* can_resize)(const clap_plugin_t * plugin);
            _Bool (* get_resize_hints)(const clap_plugin_t * plugin,
                                       clap_gui_resize_hints_t * hints);
            _Bool (* adjust_size)(const clap_plugin_t * plugin,
                                  uint32_t * width,
                                  uint32_t * height);
            _Bool (* set_size)(const clap_plugin_t * plugin,
                               uint32_t width,
                               uint32_t height);
            _Bool (* set_parent)(const clap_plugin_t * plugin,
                                 const clap_window_t * window);
            _Bool (* set_transient)(const clap_plugin_t * plugin,
                                    const clap_window_t * window);
            void (* suggest_title)(const clap_plugin_t * plugin,
                                   const char * title);
            _Bool (* show)(const clap_plugin_t * plugin);
            _Bool (* hide)(const clap_plugin_t * plugin);
        } clap_plugin_gui_t; -}

{-# LINE 80 "src/Clap/Interface/Extension/Foreign/Gui.hsc" #-}

{-# LINE 81 "src/Clap/Interface/Extension/Foreign/Gui.hsc" #-}

{-# LINE 82 "src/Clap/Interface/Extension/Foreign/Gui.hsc" #-}

{-# LINE 83 "src/Clap/Interface/Extension/Foreign/Gui.hsc" #-}

{-# LINE 84 "src/Clap/Interface/Extension/Foreign/Gui.hsc" #-}

{-# LINE 85 "src/Clap/Interface/Extension/Foreign/Gui.hsc" #-}

{-# LINE 86 "src/Clap/Interface/Extension/Foreign/Gui.hsc" #-}

{-# LINE 87 "src/Clap/Interface/Extension/Foreign/Gui.hsc" #-}

{-# LINE 88 "src/Clap/Interface/Extension/Foreign/Gui.hsc" #-}

{-# LINE 89 "src/Clap/Interface/Extension/Foreign/Gui.hsc" #-}

{-# LINE 90 "src/Clap/Interface/Extension/Foreign/Gui.hsc" #-}

{-# LINE 91 "src/Clap/Interface/Extension/Foreign/Gui.hsc" #-}

{-# LINE 92 "src/Clap/Interface/Extension/Foreign/Gui.hsc" #-}

{-# LINE 93 "src/Clap/Interface/Extension/Foreign/Gui.hsc" #-}

{-# LINE 94 "src/Clap/Interface/Extension/Foreign/Gui.hsc" #-}

{-# LINE 95 "src/Clap/Interface/Extension/Foreign/Gui.hsc" #-}
data C'clap_plugin_gui = C'clap_plugin_gui{
  c'clap_plugin_gui'is_api_supported :: FunPtr (Ptr C'clap_plugin -> CString -> CBool -> CBool),
  c'clap_plugin_gui'get_preferred_api :: FunPtr (Ptr C'clap_plugin -> Ptr CString -> Ptr CBool -> CBool),
  c'clap_plugin_gui'create :: FunPtr (Ptr C'clap_plugin -> CString -> CBool -> CBool),
  c'clap_plugin_gui'destroy :: FunPtr (Ptr C'clap_plugin -> IO ()),
  c'clap_plugin_gui'set_scale :: FunPtr (Ptr C'clap_plugin -> CDouble -> CBool),
  c'clap_plugin_gui'get_size :: FunPtr (Ptr C'clap_plugin -> Ptr CUInt -> Ptr CUInt -> CBool),
  c'clap_plugin_gui'can_resize :: FunPtr (Ptr C'clap_plugin -> CBool),
  c'clap_plugin_gui'get_resize_hints :: FunPtr (Ptr C'clap_plugin -> Ptr C'clap_gui_resize_hints -> CBool),
  c'clap_plugin_gui'adjust_size :: FunPtr (Ptr C'clap_plugin -> Ptr CUInt -> Ptr CUInt -> CBool),
  c'clap_plugin_gui'set_size :: FunPtr (Ptr C'clap_plugin -> CUInt -> CUInt -> CBool),
  c'clap_plugin_gui'set_parent :: FunPtr (Ptr C'clap_plugin -> Ptr C'clap_window -> CBool),
  c'clap_plugin_gui'set_transient :: FunPtr (Ptr C'clap_plugin -> Ptr C'clap_window -> CBool),
  c'clap_plugin_gui'suggest_title :: FunPtr (Ptr C'clap_plugin -> CString -> IO ()),
  c'clap_plugin_gui'show :: FunPtr (Ptr C'clap_plugin -> CBool),
  c'clap_plugin_gui'hide :: FunPtr (Ptr C'clap_plugin -> CBool)
} deriving (Eq,Show)
p'clap_plugin_gui'is_api_supported p = plusPtr p 0
p'clap_plugin_gui'is_api_supported :: Ptr (C'clap_plugin_gui) -> Ptr (FunPtr (Ptr C'clap_plugin -> CString -> CBool -> CBool))
p'clap_plugin_gui'get_preferred_api p = plusPtr p 8
p'clap_plugin_gui'get_preferred_api :: Ptr (C'clap_plugin_gui) -> Ptr (FunPtr (Ptr C'clap_plugin -> Ptr CString -> Ptr CBool -> CBool))
p'clap_plugin_gui'create p = plusPtr p 16
p'clap_plugin_gui'create :: Ptr (C'clap_plugin_gui) -> Ptr (FunPtr (Ptr C'clap_plugin -> CString -> CBool -> CBool))
p'clap_plugin_gui'destroy p = plusPtr p 24
p'clap_plugin_gui'destroy :: Ptr (C'clap_plugin_gui) -> Ptr (FunPtr (Ptr C'clap_plugin -> IO ()))
p'clap_plugin_gui'set_scale p = plusPtr p 32
p'clap_plugin_gui'set_scale :: Ptr (C'clap_plugin_gui) -> Ptr (FunPtr (Ptr C'clap_plugin -> CDouble -> CBool))
p'clap_plugin_gui'get_size p = plusPtr p 40
p'clap_plugin_gui'get_size :: Ptr (C'clap_plugin_gui) -> Ptr (FunPtr (Ptr C'clap_plugin -> Ptr CUInt -> Ptr CUInt -> CBool))
p'clap_plugin_gui'can_resize p = plusPtr p 48
p'clap_plugin_gui'can_resize :: Ptr (C'clap_plugin_gui) -> Ptr (FunPtr (Ptr C'clap_plugin -> CBool))
p'clap_plugin_gui'get_resize_hints p = plusPtr p 56
p'clap_plugin_gui'get_resize_hints :: Ptr (C'clap_plugin_gui) -> Ptr (FunPtr (Ptr C'clap_plugin -> Ptr C'clap_gui_resize_hints -> CBool))
p'clap_plugin_gui'adjust_size p = plusPtr p 64
p'clap_plugin_gui'adjust_size :: Ptr (C'clap_plugin_gui) -> Ptr (FunPtr (Ptr C'clap_plugin -> Ptr CUInt -> Ptr CUInt -> CBool))
p'clap_plugin_gui'set_size p = plusPtr p 72
p'clap_plugin_gui'set_size :: Ptr (C'clap_plugin_gui) -> Ptr (FunPtr (Ptr C'clap_plugin -> CUInt -> CUInt -> CBool))
p'clap_plugin_gui'set_parent p = plusPtr p 80
p'clap_plugin_gui'set_parent :: Ptr (C'clap_plugin_gui) -> Ptr (FunPtr (Ptr C'clap_plugin -> Ptr C'clap_window -> CBool))
p'clap_plugin_gui'set_transient p = plusPtr p 88
p'clap_plugin_gui'set_transient :: Ptr (C'clap_plugin_gui) -> Ptr (FunPtr (Ptr C'clap_plugin -> Ptr C'clap_window -> CBool))
p'clap_plugin_gui'suggest_title p = plusPtr p 96
p'clap_plugin_gui'suggest_title :: Ptr (C'clap_plugin_gui) -> Ptr (FunPtr (Ptr C'clap_plugin -> CString -> IO ()))
p'clap_plugin_gui'show p = plusPtr p 104
p'clap_plugin_gui'show :: Ptr (C'clap_plugin_gui) -> Ptr (FunPtr (Ptr C'clap_plugin -> CBool))
p'clap_plugin_gui'hide p = plusPtr p 112
p'clap_plugin_gui'hide :: Ptr (C'clap_plugin_gui) -> Ptr (FunPtr (Ptr C'clap_plugin -> CBool))
instance Storable C'clap_plugin_gui where
  sizeOf _ = 120
  alignment _ = 8
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 8
    v2 <- peekByteOff _p 16
    v3 <- peekByteOff _p 24
    v4 <- peekByteOff _p 32
    v5 <- peekByteOff _p 40
    v6 <- peekByteOff _p 48
    v7 <- peekByteOff _p 56
    v8 <- peekByteOff _p 64
    v9 <- peekByteOff _p 72
    v10 <- peekByteOff _p 80
    v11 <- peekByteOff _p 88
    v12 <- peekByteOff _p 96
    v13 <- peekByteOff _p 104
    v14 <- peekByteOff _p 112
    return $ C'clap_plugin_gui v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14
  poke _p (C'clap_plugin_gui v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 8 v1
    pokeByteOff _p 16 v2
    pokeByteOff _p 24 v3
    pokeByteOff _p 32 v4
    pokeByteOff _p 40 v5
    pokeByteOff _p 48 v6
    pokeByteOff _p 56 v7
    pokeByteOff _p 64 v8
    pokeByteOff _p 72 v9
    pokeByteOff _p 80 v10
    pokeByteOff _p 88 v11
    pokeByteOff _p 96 v12
    pokeByteOff _p 104 v13
    pokeByteOff _p 112 v14
    return ()

{-# LINE 96 "src/Clap/Interface/Extension/Foreign/Gui.hsc" #-}
type C'clap_plugin_gui_t = C'clap_plugin_gui

{-# LINE 97 "src/Clap/Interface/Extension/Foreign/Gui.hsc" #-}
{- typedef struct clap_host_gui {
            void (* resize_hints_changed)(const clap_host_t * host);
            _Bool (* request_resize)(const clap_host_t * host,
                                     uint32_t width,
                                     uint32_t height);
            _Bool (* request_show)(const clap_host_t * host);
            _Bool (* request_hide)(const clap_host_t * host);
            void (* closed)(const clap_host_t * host, _Bool was_destroyed);
        } clap_host_gui_t; -}

{-# LINE 107 "src/Clap/Interface/Extension/Foreign/Gui.hsc" #-}

{-# LINE 108 "src/Clap/Interface/Extension/Foreign/Gui.hsc" #-}

{-# LINE 109 "src/Clap/Interface/Extension/Foreign/Gui.hsc" #-}

{-# LINE 110 "src/Clap/Interface/Extension/Foreign/Gui.hsc" #-}

{-# LINE 111 "src/Clap/Interface/Extension/Foreign/Gui.hsc" #-}

{-# LINE 112 "src/Clap/Interface/Extension/Foreign/Gui.hsc" #-}
data C'clap_host_gui = C'clap_host_gui{
  c'clap_host_gui'resize_hints_changed :: FunPtr (Ptr C'clap_host -> IO ()),
  c'clap_host_gui'request_resize :: FunPtr (Ptr C'clap_host -> CUInt -> CUInt -> CInt),
  c'clap_host_gui'request_show :: FunPtr (Ptr C'clap_host -> CInt),
  c'clap_host_gui'request_hide :: FunPtr (Ptr C'clap_host -> CInt),
  c'clap_host_gui'closed :: FunPtr (Ptr C'clap_host -> CInt -> IO ())
} deriving (Eq,Show)
p'clap_host_gui'resize_hints_changed p = plusPtr p 0
p'clap_host_gui'resize_hints_changed :: Ptr (C'clap_host_gui) -> Ptr (FunPtr (Ptr C'clap_host -> IO ()))
p'clap_host_gui'request_resize p = plusPtr p 8
p'clap_host_gui'request_resize :: Ptr (C'clap_host_gui) -> Ptr (FunPtr (Ptr C'clap_host -> CUInt -> CUInt -> CInt))
p'clap_host_gui'request_show p = plusPtr p 16
p'clap_host_gui'request_show :: Ptr (C'clap_host_gui) -> Ptr (FunPtr (Ptr C'clap_host -> CInt))
p'clap_host_gui'request_hide p = plusPtr p 24
p'clap_host_gui'request_hide :: Ptr (C'clap_host_gui) -> Ptr (FunPtr (Ptr C'clap_host -> CInt))
p'clap_host_gui'closed p = plusPtr p 32
p'clap_host_gui'closed :: Ptr (C'clap_host_gui) -> Ptr (FunPtr (Ptr C'clap_host -> CInt -> IO ()))
instance Storable C'clap_host_gui where
  sizeOf _ = 40
  alignment _ = 8
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 8
    v2 <- peekByteOff _p 16
    v3 <- peekByteOff _p 24
    v4 <- peekByteOff _p 32
    return $ C'clap_host_gui v0 v1 v2 v3 v4
  poke _p (C'clap_host_gui v0 v1 v2 v3 v4) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 8 v1
    pokeByteOff _p 16 v2
    pokeByteOff _p 24 v3
    pokeByteOff _p 32 v4
    return ()

{-# LINE 113 "src/Clap/Interface/Extension/Foreign/Gui.hsc" #-}
type C'clap_host_gui_t = C'clap_host_gui

{-# LINE 114 "src/Clap/Interface/Extension/Foreign/Gui.hsc" #-}
type C'is_api_supported = FunPtr (Ptr C'clap_plugin -> CString -> CBool -> CBool)
foreign import ccall "wrapper" mk'is_api_supported
  :: (Ptr C'clap_plugin -> CString -> CBool -> CBool) -> IO C'is_api_supported
foreign import ccall "dynamic" mK'is_api_supported
  :: C'is_api_supported -> (Ptr C'clap_plugin -> CString -> CBool -> CBool)

{-# LINE 115 "src/Clap/Interface/Extension/Foreign/Gui.hsc" #-}
type C'get_preferred_api = FunPtr (Ptr C'clap_plugin -> Ptr CString -> Ptr CBool -> CBool)
foreign import ccall "wrapper" mk'get_preferred_api
  :: (Ptr C'clap_plugin -> Ptr CString -> Ptr CBool -> CBool) -> IO C'get_preferred_api
foreign import ccall "dynamic" mK'get_preferred_api
  :: C'get_preferred_api -> (Ptr C'clap_plugin -> Ptr CString -> Ptr CBool -> CBool)

{-# LINE 116 "src/Clap/Interface/Extension/Foreign/Gui.hsc" #-}
type C'create = FunPtr (Ptr C'clap_plugin -> CString -> CBool -> CBool)
foreign import ccall "wrapper" mk'create
  :: (Ptr C'clap_plugin -> CString -> CBool -> CBool) -> IO C'create
foreign import ccall "dynamic" mK'create
  :: C'create -> (Ptr C'clap_plugin -> CString -> CBool -> CBool)

{-# LINE 117 "src/Clap/Interface/Extension/Foreign/Gui.hsc" #-}
type C'destroy = FunPtr (Ptr C'clap_plugin -> IO ())
foreign import ccall "wrapper" mk'destroy
  :: (Ptr C'clap_plugin -> IO ()) -> IO C'destroy
foreign import ccall "dynamic" mK'destroy
  :: C'destroy -> (Ptr C'clap_plugin -> IO ())

{-# LINE 118 "src/Clap/Interface/Extension/Foreign/Gui.hsc" #-}
type C'set_scale = FunPtr (Ptr C'clap_plugin -> CDouble -> CBool)
foreign import ccall "wrapper" mk'set_scale
  :: (Ptr C'clap_plugin -> CDouble -> CBool) -> IO C'set_scale
foreign import ccall "dynamic" mK'set_scale
  :: C'set_scale -> (Ptr C'clap_plugin -> CDouble -> CBool)

{-# LINE 119 "src/Clap/Interface/Extension/Foreign/Gui.hsc" #-}
type C'get_size = FunPtr (Ptr C'clap_plugin -> Ptr CUInt -> Ptr CUInt -> CBool)
foreign import ccall "wrapper" mk'get_size
  :: (Ptr C'clap_plugin -> Ptr CUInt -> Ptr CUInt -> CBool) -> IO C'get_size
foreign import ccall "dynamic" mK'get_size
  :: C'get_size -> (Ptr C'clap_plugin -> Ptr CUInt -> Ptr CUInt -> CBool)

{-# LINE 120 "src/Clap/Interface/Extension/Foreign/Gui.hsc" #-}
type C'can_resize = FunPtr (Ptr C'clap_plugin -> CBool)
foreign import ccall "wrapper" mk'can_resize
  :: (Ptr C'clap_plugin -> CBool) -> IO C'can_resize
foreign import ccall "dynamic" mK'can_resize
  :: C'can_resize -> (Ptr C'clap_plugin -> CBool)

{-# LINE 121 "src/Clap/Interface/Extension/Foreign/Gui.hsc" #-}
type C'get_resize_hints = FunPtr (Ptr C'clap_plugin -> Ptr C'clap_gui_resize_hints -> CBool)
foreign import ccall "wrapper" mk'get_resize_hints
  :: (Ptr C'clap_plugin -> Ptr C'clap_gui_resize_hints -> CBool) -> IO C'get_resize_hints
foreign import ccall "dynamic" mK'get_resize_hints
  :: C'get_resize_hints -> (Ptr C'clap_plugin -> Ptr C'clap_gui_resize_hints -> CBool)

{-# LINE 122 "src/Clap/Interface/Extension/Foreign/Gui.hsc" #-}
type C'adjust_size = FunPtr (Ptr C'clap_plugin -> Ptr CUInt -> Ptr CUInt -> CBool)
foreign import ccall "wrapper" mk'adjust_size
  :: (Ptr C'clap_plugin -> Ptr CUInt -> Ptr CUInt -> CBool) -> IO C'adjust_size
foreign import ccall "dynamic" mK'adjust_size
  :: C'adjust_size -> (Ptr C'clap_plugin -> Ptr CUInt -> Ptr CUInt -> CBool)

{-# LINE 123 "src/Clap/Interface/Extension/Foreign/Gui.hsc" #-}
type C'set_size = FunPtr (Ptr C'clap_plugin -> CUInt -> CUInt -> CBool)
foreign import ccall "wrapper" mk'set_size
  :: (Ptr C'clap_plugin -> CUInt -> CUInt -> CBool) -> IO C'set_size
foreign import ccall "dynamic" mK'set_size
  :: C'set_size -> (Ptr C'clap_plugin -> CUInt -> CUInt -> CBool)

{-# LINE 124 "src/Clap/Interface/Extension/Foreign/Gui.hsc" #-}
type C'set_parent = FunPtr (Ptr C'clap_plugin -> Ptr C'clap_window -> CBool)
foreign import ccall "wrapper" mk'set_parent
  :: (Ptr C'clap_plugin -> Ptr C'clap_window -> CBool) -> IO C'set_parent
foreign import ccall "dynamic" mK'set_parent
  :: C'set_parent -> (Ptr C'clap_plugin -> Ptr C'clap_window -> CBool)

{-# LINE 125 "src/Clap/Interface/Extension/Foreign/Gui.hsc" #-}
type C'set_transient = FunPtr (Ptr C'clap_plugin -> Ptr C'clap_window -> CBool)
foreign import ccall "wrapper" mk'set_transient
  :: (Ptr C'clap_plugin -> Ptr C'clap_window -> CBool) -> IO C'set_transient
foreign import ccall "dynamic" mK'set_transient
  :: C'set_transient -> (Ptr C'clap_plugin -> Ptr C'clap_window -> CBool)

{-# LINE 126 "src/Clap/Interface/Extension/Foreign/Gui.hsc" #-}
type C'suggest_title = FunPtr (Ptr C'clap_plugin -> CString -> IO ())
foreign import ccall "wrapper" mk'suggest_title
  :: (Ptr C'clap_plugin -> CString -> IO ()) -> IO C'suggest_title
foreign import ccall "dynamic" mK'suggest_title
  :: C'suggest_title -> (Ptr C'clap_plugin -> CString -> IO ())

{-# LINE 127 "src/Clap/Interface/Extension/Foreign/Gui.hsc" #-}
type C'show = FunPtr (Ptr C'clap_plugin -> CBool)
foreign import ccall "wrapper" mk'show
  :: (Ptr C'clap_plugin -> CBool) -> IO C'show
foreign import ccall "dynamic" mK'show
  :: C'show -> (Ptr C'clap_plugin -> CBool)

{-# LINE 128 "src/Clap/Interface/Extension/Foreign/Gui.hsc" #-}
type C'hide = FunPtr (Ptr C'clap_plugin -> CBool)
foreign import ccall "wrapper" mk'hide
  :: (Ptr C'clap_plugin -> CBool) -> IO C'hide
foreign import ccall "dynamic" mK'hide
  :: C'hide -> (Ptr C'clap_plugin -> CBool)
