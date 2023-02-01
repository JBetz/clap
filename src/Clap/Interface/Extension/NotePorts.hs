module Clap.Interface.Extension.NotePorts where

import Clap.Interface.Id
import Clap.Interface.Foreign
import Clap.Interface.Host
import Clap.Interface.Plugin
import Clap.Interface.Extension.Foreign.NotePorts
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

data NoteDialect
    = NoteDialectClap
    | NoteDialectMidi
    | NoteDialectMidiMpe
    | MoteDialectMidi2
    deriving (Enum, Bounded, Show)

data NotePortInfo = NotePortInfo
    { notePortInfo_id :: ClapId 
    , notePortInfo_supportedDialects :: [NoteDialect]
    , notePortInfo_preferredDialect :: NoteDialect
    , notePortInfo_name :: String
    } deriving (Show)

type PluginNotePortsHandle = Ptr C'clap_plugin_note_ports

count :: PluginNotePortsHandle -> PluginHandle -> Bool -> IO Word32 
count pluginNotePorts plugin isInput = do
    funPtr <- peek $ p'clap_plugin_note_ports'count pluginNotePorts
    pure $ fromIntegral $ mK'count funPtr plugin (fromBool isInput)

get :: PluginNotePortsHandle -> PluginHandle -> Word32 -> Bool -> IO (Maybe NotePortInfo)
get pluginNotePorts plugin index isInput = do
    funPtr <- peek $ p'clap_plugin_note_ports'get pluginNotePorts
    cNotePortInfoPtr <- new $ C'clap_note_port_info 
        { c'clap_note_port_info'id = 0
        , c'clap_note_port_info'supported_dialects = 0
        , c'clap_note_port_info'preferred_dialect = 0
        , c'clap_note_port_info'name = [CChar 0]
        }
    let result = toBool $ mK'get funPtr plugin (fromIntegral index) (fromBool isInput) cNotePortInfoPtr
    cNotePortInfo <- peek cNotePortInfoPtr
    if result
        then pure $ Just $ NotePortInfo
            { notePortInfo_id = ClapId $ fromIntegral $ c'clap_note_port_info'id cNotePortInfo
            , notePortInfo_supportedDialects = intToFlags $ fromIntegral $ c'clap_note_port_info'supported_dialects cNotePortInfo
            , notePortInfo_preferredDialect = toEnum $ fromIntegral $ c'clap_note_port_info'preferred_dialect cNotePortInfo
            , notePortInfo_name = castCCharToChar <$> c'clap_note_port_info'name cNotePortInfo
            }
        else pure Nothing

data RescanFlag
    = RescanFlag_All
    | RescanFlag_Names
    deriving (Enum, Bounded)

type HostNotePortsHandle = Ptr C'clap_host_note_ports

supportedDialects :: HostNotePortsHandle -> HostHandle -> IO [NoteDialect] 
supportedDialects hostNotePorts host = do
    funPtr <- peek $ p'clap_host_note_ports'supported_dialects hostNotePorts
    pure $ intToFlags $ fromIntegral $ mK'supported_dialects funPtr host

rescan :: HostNotePortsHandle -> HostHandle -> [RescanFlag] -> IO ()
rescan hostNotePorts host rescanFlags = do
    funPtr <- peek $ p'clap_host_note_ports'rescan hostNotePorts
    mK'rescan funPtr host (fromIntegral $ flagsToInt rescanFlags)

