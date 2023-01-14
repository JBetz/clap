{-# LANGUAGE LambdaCase #-}

module Clap.Interface.Extension.AudioPorts where

import Clap.Interface.Extension.Foreign.AudioPorts
import Clap.Interface.Foreign
import Clap.Interface.Host
import Clap.Interface.Id
import Clap.Interface.Plugin
import Data.Int
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Text.Read

data AudioPortFlag
    = IsMain 
    | Supports64Bits
    | Prefers64Bits
    | RequiresCommonSampleSize
    deriving (Enum, Bounded, Show)

data PortType
    = Mono
    | Stereo
    | Surround  -- ext/surround
    | Ambisonic -- ext/ambisonic 
    | CV        -- ext/cv
    deriving (Enum, Bounded)

instance Show PortType where
    show = \case
        Mono -> "mono"
        Stereo -> "stereo"
        Surround -> "surround"
        Ambisonic -> "ambisonic"
        CV -> "cv"

instance Read PortType where
    readPrec = do
        Ident string <- lexP
        case string of
            "mono" -> pure Mono
            "stereo" -> pure Stereo
            "surround" -> pure Surround
            "ambisonic" -> pure Ambisonic
            "cv" -> pure CV
    readListPrec = readListPrecDefault


data AudioPortInfo = AudioPortInfo
    { audioPortInfo_id :: ClapId
    , audioPortInfo_name :: String
    , audioPortInfo_flags :: [AudioPortFlag]
    , audioPortInfo_channelCount :: Int32
    , audioPortInfo_portType :: PortType
    , audioPortInfo_inPlacePair :: ClapId 
    }

type PluginAudioPortsHandle = Ptr C'clap_plugin_audio_ports

count :: PluginAudioPortsHandle -> PluginHandle -> Bool -> IO Int32
count pluginAudioPorts plugin isInput = do
    funPtr <- peek $ p'clap_plugin_audio_ports'count pluginAudioPorts
    pure $ fromIntegral $ mK'count funPtr plugin (fromBool isInput)

get :: PluginAudioPortsHandle -> PluginHandle -> Int32 -> Bool -> IO (Maybe AudioPortInfo)
get pluginAudioPorts plugin index isInput = do
    funPtr <- peek $ p'clap_plugin_audio_ports'get pluginAudioPorts
    cAudioPortInfoPtr <- new $ C'clap_audio_port_info 
        { c'clap_audio_port_info'id = 0 
        , c'clap_audio_port_info'name = [CChar 0]
        , c'clap_audio_port_info'flags = 0
        , c'clap_audio_port_info'channel_count = 0
        , c'clap_audio_port_info'port_type = nullPtr
        , c'clap_audio_port_info'in_place_pair = 0
        }
    let isSuccessful = toBool $ mK'get funPtr plugin (fromIntegral index) (fromBool isInput) cAudioPortInfoPtr
    if isSuccessful
        then do
            cAudioPortInfo <- peek cAudioPortInfoPtr
            name <- peekCString $ p'clap_audio_port_info'name cAudioPortInfoPtr
            portType <- peekCString $ c'clap_audio_port_info'port_type cAudioPortInfo
            pure $ Just $ AudioPortInfo
                { audioPortInfo_id = ClapId $ fromIntegral $ c'clap_audio_port_info'id cAudioPortInfo
                , audioPortInfo_name = name
                , audioPortInfo_flags = intToFlags $ fromIntegral $ c'clap_audio_port_info'flags cAudioPortInfo
                , audioPortInfo_channelCount = fromIntegral $ c'clap_audio_port_info'channel_count cAudioPortInfo
                , audioPortInfo_portType = read portType
                , audioPortInfo_inPlacePair = ClapId $ fromIntegral $ c'clap_audio_port_info'in_place_pair cAudioPortInfo
                }
        else pure Nothing


data RescanFlag
    = RescanNames
    | RescanFlags
    | RescanChannelCount
    | RescanPortType
    | RescanInPlacePair
    | RescanList
    deriving (Enum, Bounded)

type HostAudioPortsHandle = Ptr C'clap_host_audio_ports

isRescanFlagSupported :: HostAudioPortsHandle -> HostHandle -> RescanFlag -> IO Bool
isRescanFlagSupported hostAudioPorts host flag = do
    funPtr <- peek $ p'clap_host_audio_ports'is_rescan_flag_supported hostAudioPorts
    pure $ toBool $ mK'is_rescan_flag_supported funPtr host (fromIntegral $ flagsToInt [flag])

rescan :: HostAudioPortsHandle -> HostHandle -> [RescanFlag] -> IO ()
rescan hostAudioPorts host flags = do 
    funPtr <- peek $ p'clap_host_audio_ports'rescan hostAudioPorts
    mK'rescan funPtr host (fromIntegral $ flagsToInt flags)
