{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <clap/ext/posix-fd-support.h>
module Clap.Interface.Extension.Foreign.PosixFdSupport where
import Foreign.Ptr
#strict_import

import Clap.Interface.Extension.Foreign....Plugin
#globalarray CLAP_EXT_POSIX_FD_SUPPORT , CChar
{- enum {
    CLAP_POSIX_FD_READ = 1 << 0,
    CLAP_POSIX_FD_WRITE = 1 << 1,
    CLAP_POSIX_FD_ERROR = 1 << 2
}; -}
#num CLAP_POSIX_FD_READ
#num CLAP_POSIX_FD_WRITE
#num CLAP_POSIX_FD_ERROR
{- typedef uint32_t clap_posix_fd_flags_t; -}
#synonym_t clap_posix_fd_flags_t , CUInt
{- typedef struct clap_plugin_posix_fd_support {
            void (* on_fd)(const clap_plugin_t * plugin,
                           int fd,
                           clap_posix_fd_flags_t flags);
        } clap_plugin_posix_fd_support_t; -}
#starttype struct clap_plugin_posix_fd_support
#field on_fd , FunPtr (Ptr <struct clap_plugin> -> CInt -> CUInt -> IO ())
#stoptype
#synonym_t clap_plugin_posix_fd_support_t , <struct clap_plugin_posix_fd_support>
{- typedef struct clap_host_posix_fd_support {
            _Bool (* register_fd)(const clap_host_t * host,
                                  int fd,
                                  clap_posix_fd_flags_t flags);
            _Bool (* modify_fd)(const clap_host_t * host,
                                int fd,
                                clap_posix_fd_flags_t flags);
            _Bool (* unregister_fd)(const clap_host_t * host, int fd);
        } clap_host_posix_fd_support_t; -}
#starttype struct clap_host_posix_fd_support
#field register_fd , FunPtr (Ptr <struct clap_host> -> CInt -> CUInt -> CInt)
#field modify_fd , FunPtr (Ptr <struct clap_host> -> CInt -> CUInt -> CInt)
#field unregister_fd , FunPtr (Ptr <struct clap_host> -> CInt -> CInt)
#stoptype
#synonym_t clap_host_posix_fd_support_t , <struct clap_host_posix_fd_support>
