#include <clap/ext/audio-ports.h>
#include <clap/ext/gui.h>
#include <clap/plugin-invalidation.h>

char *array_CLAP_WINDOW_API_WIN32 (void) {return CLAP_WINDOW_API_WIN32;}
char *array_CLAP_WINDOW_API_COCOA (void) {return CLAP_WINDOW_API_COCOA;}
char *array_CLAP_WINDOW_API_X11 (void) {return CLAP_WINDOW_API_X11;}
char *array_CLAP_WINDOW_API_WAYLAND (void) {return CLAP_WINDOW_API_WAYLAND;}
char *array_CLAP_PLUGIN_INVALIDATION_FACTORY_ID (void) {return CLAP_PLUGIN_INVALIDATION_FACTORY_ID;}
char *array_CLAP_EXT_AUDIO_PORTS (void) {return CLAP_EXT_AUDIO_PORTS;}
char *array_CLAP_PORT_MONO (void) {return CLAP_PORT_MONO;}
char *array_CLAP_PORT_STEREO (void) {return CLAP_PORT_STEREO;}