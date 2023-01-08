{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <ext/gui.h>
module Clap.Interface.Extension.Foreign.Gui where
import Foreign.Ptr
#strict_import

import Clap.Interface.Foreign.Host
import Clap.Interface.Foreign.Plugin (C'clap_plugin)
-- #globalarray CLAP_EXT_GUI , CChar
-- #globalarray CLAP_WINDOW_API_WIN32 , CChar
-- #globalarray CLAP_WINDOW_API_COCOA , CChar
-- #globalarray CLAP_WINDOW_API_X11 , CChar
-- #globalarray CLAP_WINDOW_API_WAYLAND , CChar
-- #globalvar clap_hwnd , Ptr ()
-- #globalvar clap_nsview , Ptr ()
{- typedef unsigned long clap_xwnd; -}
#synonym_t clap_xwnd , CULong
{- typedef struct clap_window {
            const char * api;
            union {
                clap_nsview cocoa; clap_xwnd x11; clap_hwnd win32; void * ptr;
            };
        } clap_window_t; -}
#starttype struct clap_window
#field api , CString
#field handle , Ptr ()
#stoptype
#synonym_t clap_window_t , <struct clap_window>
{- typedef struct clap_gui_resize_hints {
            _Bool can_resize_horizontally;
            _Bool can_resize_vertically;
            _Bool preserve_aspect_ratio;
            uint32_t aspect_ratio_width;
            uint32_t aspect_ratio_height;
        } clap_gui_resize_hints_t; -}
#starttype struct clap_gui_resize_hints
#field can_resize_horizontally , CInt
#field can_resize_vertically , CInt
#field preserve_aspect_ratio , CInt
#field aspect_ratio_width , CUInt
#field aspect_ratio_height , CUInt
#stoptype
#synonym_t clap_gui_resize_hints_t , <struct clap_gui_resize_hints>
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
#starttype struct clap_plugin_gui
#field is_api_supported , FunPtr (Ptr <struct clap_plugin> -> CString -> CBool -> CBool)
#field get_preferred_api , FunPtr (Ptr <struct clap_plugin> -> Ptr CString -> Ptr CBool -> CBool)
#field create , FunPtr (Ptr <struct clap_plugin> -> CString -> CBool -> CBool)
#field destroy , FunPtr (Ptr <struct clap_plugin> -> IO ())
#field set_scale , FunPtr (Ptr <struct clap_plugin> -> CDouble -> CBool)
#field get_size , FunPtr (Ptr <struct clap_plugin> -> Ptr CUInt -> Ptr CUInt -> CInt)
#field can_resize , FunPtr (Ptr <struct clap_plugin> -> CBool)
#field get_resize_hints , FunPtr (Ptr <struct clap_plugin> -> Ptr <struct clap_gui_resize_hints> -> CBool)
#field adjust_size , FunPtr (Ptr <struct clap_plugin> -> Ptr CUInt -> Ptr CUInt -> CBool)
#field set_size , FunPtr (Ptr <struct clap_plugin> -> CUInt -> CUInt -> CBool)
#field set_parent , FunPtr (Ptr <struct clap_plugin> -> Ptr <struct clap_window> -> CBool)
#field set_transient , FunPtr (Ptr <struct clap_plugin> -> Ptr <struct clap_window> -> CBool)
#field suggest_title , FunPtr (Ptr <struct clap_plugin> -> CString -> IO ())
#field show , FunPtr (Ptr <struct clap_plugin> -> CBool)
#field hide , FunPtr (Ptr <struct clap_plugin> -> CBool)
#stoptype
#synonym_t clap_plugin_gui_t , <struct clap_plugin_gui>
{- typedef struct clap_host_gui {
            void (* resize_hints_changed)(const clap_host_t * host);
            _Bool (* request_resize)(const clap_host_t * host,
                                     uint32_t width,
                                     uint32_t height);
            _Bool (* request_show)(const clap_host_t * host);
            _Bool (* request_hide)(const clap_host_t * host);
            void (* closed)(const clap_host_t * host, _Bool was_destroyed);
        } clap_host_gui_t; -}
#starttype struct clap_host_gui
#field resize_hints_changed , FunPtr (Ptr <struct clap_host> -> IO ())
#field request_resize , FunPtr (Ptr <struct clap_host> -> CUInt -> CUInt -> CInt)
#field request_show , FunPtr (Ptr <struct clap_host> -> CInt)
#field request_hide , FunPtr (Ptr <struct clap_host> -> CInt)
#field closed , FunPtr (Ptr <struct clap_host> -> CInt -> IO ())
#stoptype
#synonym_t clap_host_gui_t , <struct clap_host_gui>
#callback_t is_api_supported , Ptr <struct clap_plugin> -> CString -> CBool -> CBool
#callback_t get_preferred_api , Ptr <struct clap_plugin> -> Ptr CString -> Ptr CBool -> CBool
#callback_t create , Ptr <struct clap_plugin> -> CString -> CBool -> CBool
#callback_t destroy , Ptr <struct clap_plugin> -> IO ()
#callback_t set_scale , Ptr <struct clap_plugin> -> CDouble -> CBool
#callback_t get_size , Ptr <struct clap_plugin> -> Ptr CUInt -> Ptr CUInt -> CInt
#callback_t can_resize , Ptr <struct clap_plugin> -> CBool
#callback_t get_resize_hints , Ptr <struct clap_plugin> -> Ptr <struct clap_gui_resize_hints> -> CBool
#callback_t adjust_size , Ptr <struct clap_plugin> -> Ptr CUInt -> Ptr CUInt -> CBool
#callback_t set_size , Ptr <struct clap_plugin> -> CUInt -> CUInt -> CBool
#callback_t set_parent , Ptr <struct clap_plugin> -> Ptr <struct clap_window> -> CBool
#callback_t set_transient , Ptr <struct clap_plugin> -> Ptr <struct clap_window> -> CBool
#callback_t suggest_title , Ptr <struct clap_plugin> -> CString -> IO ()
#callback_t show , Ptr <struct clap_plugin> -> CBool
#callback_t hide , Ptr <struct clap_plugin> -> CBool