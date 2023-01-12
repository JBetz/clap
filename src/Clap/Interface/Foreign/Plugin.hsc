{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <clap/plugin.h>
module Clap.Interface.Foreign.Plugin where
import Foreign.Ptr
#strict_import

import Clap.Interface.Foreign.Host (C'clap_host)
import Clap.Interface.Foreign.Process (C'clap_process, C'clap_process_status)
import Clap.Interface.Foreign.Version
{- typedef struct clap_plugin_descriptor {
            clap_version_t clap_version;
            const char * id;
            const char * name;
            const char * vendor;
            const char * url;
            const char * manual_url;
            const char * support_url;
            const char * version;
            const char * description;
            const char * const * features;
        } clap_plugin_descriptor_t; -}
#starttype struct clap_plugin_descriptor
#field clap_version , <struct clap_version>
#field id , CString
#field name , CString
#field vendor , CString
#field url , CString
#field manual_url , CString
#field support_url , CString
#field version , CString
#field description , CString
#field features , Ptr CString
#stoptype
#synonym_t clap_plugin_descriptor_t , <struct clap_plugin_descriptor>
{- typedef struct clap_plugin {
            const clap_plugin_descriptor_t * desc;
            void * plugin_data;
            _Bool (* init)(const struct clap_plugin * plugin);
            void (* destroy)(const struct clap_plugin * plugin);
            _Bool (* activate)(const struct clap_plugin * plugin,
                               double sample_rate,
                               uint32_t min_frames_count,
                               uint32_t max_frames_count);
            void (* deactivate)(const struct clap_plugin * plugin);
            _Bool (* start_processing)(const struct clap_plugin * plugin);
            void (* stop_processing)(const struct clap_plugin * plugin);
            void (* reset)(const struct clap_plugin * plugin);
            clap_process_status (* process)(const struct clap_plugin * plugin,
                                            const clap_process_t * process);
            const void * (* get_extension)(const struct clap_plugin * plugin,
                                           const char * id);
            void (* on_main_thread)(const struct clap_plugin * plugin);
        } clap_plugin_t; -}
#starttype struct clap_plugin
#field desc , Ptr <struct clap_plugin_descriptor>
#field plugin_data , Ptr ()
#field init , FunPtr (Ptr <struct clap_plugin> -> CBool)
#field destroy , FunPtr (Ptr <struct clap_plugin> -> IO ())
#field activate , FunPtr (Ptr <struct clap_plugin> -> CDouble -> CUInt -> CUInt -> CBool)
#field deactivate , FunPtr (Ptr <struct clap_plugin> -> IO ())
#field start_processing , FunPtr (Ptr <struct clap_plugin> -> CBool)
#field stop_processing , FunPtr (Ptr <struct clap_plugin> -> IO ())
#field reset , FunPtr (Ptr <struct clap_plugin> -> IO ())
#field process , FunPtr (Ptr <struct clap_plugin> -> Ptr <struct clap_process> -> <clap_process_status>)
#field get_extension , FunPtr (Ptr <struct clap_plugin> -> CString -> Ptr ())
#field on_main_thread , FunPtr (Ptr <struct clap_plugin> -> IO ())
#stoptype
#synonym_t clap_plugin_t , <struct clap_plugin>
#callback_t init , Ptr <struct clap_plugin> -> CBool
#callback_t destroy , Ptr <struct clap_plugin> -> IO ()
#callback_t activate , Ptr <struct clap_plugin> -> CDouble -> CUInt -> CUInt -> CBool
#callback_t deactivate , Ptr <struct clap_plugin> -> IO ()
#callback_t start_processing , Ptr <struct clap_plugin> -> CBool
#callback_t stop_processing , Ptr <struct clap_plugin> -> IO ()
#callback_t reset , Ptr <struct clap_plugin> -> IO ()
#callback_t process , Ptr <struct clap_plugin> -> Ptr <struct clap_process> -> <clap_process_status>
#callback_t get_extension , Ptr <struct clap_plugin> -> CString -> Ptr ()
#callback_t on_main_thread , Ptr <struct clap_plugin> -> IO ()
