module Clap.Interface.Extension.Params where

import Clap.Interface.Extension.Foreign.Params
import Clap.Interface.Events
import Clap.Interface.Foreign
import Clap.Interface.Host
import Clap.Interface.Id
import Clap.Interface.Plugin
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

data ParameterFlag
    = IsStepped
    | IsPeriodic
    | IsHidden
    | IsReadOnly 
    | IsBypass
    | IsAutomatable
    | IsAutomatablePerNoteId
    | IsAutomatablePerKey
    | IsAutomatablePerChannel
    | IsAutomatablePerPort
    | IsModulatable
    | IsModulatablePerNoteId
    | IsModulatablePerKey
    | IsModulatablePerChannel
    | IsModulatablePerPort
    | RequiresAccess
    deriving (Enum, Bounded, Show)

type ParameterInfoHandle = Ptr C'clap_param_info

data ParameterInfo = ParameterInfo
    { parameterInfo_id :: ClapId 
    , parameterInfo_flags :: [ParameterFlag]
    , parameterInfo_cookie :: Ptr ()
    , parameterInfo_name :: String
    , parameterInfo_module :: String
    , parameterInfo_minimumValue :: Double
    , parameterInfo_maximumValue :: Double
    , parameterInfo_defaultValue :: Double
    } deriving (Show)

type PluginParametersHandle = Ptr C'clap_plugin_params

count :: PluginParametersHandle -> PluginHandle -> IO Word32
count pluginParameters plugin = do
    funPtr <- peek $ p'clap_plugin_params'count pluginParameters
    pure $ fromIntegral $ mK'count funPtr plugin

getInfo :: PluginParametersHandle -> PluginHandle -> Word32 -> IO (Maybe ParameterInfo)
getInfo pluginParameters plugin index = do
    funPtr <- peek $ p'clap_plugin_params'get_info pluginParameters
    cParamInfoPtr <- new $ C'clap_param_info 
        { c'clap_param_info'id = 0
        , c'clap_param_info'flags = 0
        , c'clap_param_info'cookie = nullPtr
        , c'clap_param_info'name = [CChar 0]
        , c'clap_param_info'module = [CChar 0]
        , c'clap_param_info'min_value = 0 
        , c'clap_param_info'max_value = 0
        , c'clap_param_info'default_value = 0
        }
    let result = toBool $ mK'get_info funPtr plugin (fromIntegral index) cParamInfoPtr
    cParamInfo <- peek cParamInfoPtr
    if result
        then do
            pure $ Just $ ParameterInfo
                { parameterInfo_id = ClapId $ fromIntegral $ c'clap_param_info'id cParamInfo
                , parameterInfo_flags = intToFlags $ fromIntegral $ c'clap_param_info'flags cParamInfo
                , parameterInfo_cookie = c'clap_param_info'cookie cParamInfo
                , parameterInfo_name = fromCChars $ c'clap_param_info'name cParamInfo
                , parameterInfo_module = fromCChars $ c'clap_param_info'module cParamInfo
                , parameterInfo_minimumValue = fromCDouble $ c'clap_param_info'min_value cParamInfo
                , parameterInfo_maximumValue = fromCDouble $ c'clap_param_info'max_value cParamInfo
                , parameterInfo_defaultValue = fromCDouble $ c'clap_param_info'default_value cParamInfo
                } 
        else pure Nothing

getValue :: PluginParametersHandle -> PluginHandle -> ClapId -> IO (Maybe Double)
getValue pluginParameters plugin (ClapId parameterId) = do
    funPtr <- peek $ p'clap_plugin_params'get_value pluginParameters
    cValue <- new 0
    let result = toBool $ mK'get_value funPtr plugin (fromIntegral parameterId) cValue
    if result
        then pure Nothing
        else Just . fromCDouble <$> peek cValue 

valueToText :: PluginParametersHandle -> PluginHandle -> ClapId -> Double -> Word32 -> IO (Maybe String)
valueToText pluginParameters plugin (ClapId parameterId) value textLength = do
    funPtr <- peek $ p'clap_plugin_params'value_to_text pluginParameters
    cText <- newCString ""
    let result = toBool $ mK'value_to_text funPtr plugin (fromIntegral parameterId) (CDouble value) cText (fromIntegral textLength)
    if result
        then pure Nothing
        else Just <$> peekCString cText 

textToValue :: PluginParametersHandle -> PluginHandle -> ClapId -> String -> IO (Maybe Double)
textToValue pluginParameters plugin (ClapId parameterId) text = do
    funPtr <- peek $ p'clap_plugin_params'text_to_value pluginParameters
    withCString text $ \cText -> do
        cDoublePtr <- new 0
        let result = toBool $ mK'text_to_value funPtr plugin (fromIntegral parameterId) cText cDoublePtr
        if result
            then pure Nothing
            else Just . fromCDouble <$> peek cDoublePtr 

flush :: PluginParametersHandle -> PluginHandle -> InputEventsHandle -> OutputEventsHandle -> IO ()
flush pluginParameters plugin inputEvents outputEvents = do
    funPtr <- peek $ p'clap_plugin_params'flush pluginParameters
    mK'flush funPtr plugin inputEvents outputEvents

data ParameterRescanFlag
    = RescanValues
    | RescanText
    | RescanInfo
    | RescanAll
    deriving (Enum, Bounded, Show)

data ParameterClearFlag
    = ClearAll
    | ClearAutomations
    | ClearModulations
    deriving (Enum, Bounded, Show)

type HostParametersHandle = Ptr C'clap_host_params

rescan :: HostParametersHandle -> HostHandle -> [ParameterRescanFlag] -> IO ()
rescan hostParameters host rescanFlags = do
    funPtr <- peek $ p'clap_host_params'rescan hostParameters
    mK'rescan funPtr host (fromIntegral $ flagsToInt rescanFlags)

clear :: HostParametersHandle -> HostHandle -> ClapId -> [ParameterClearFlag] -> IO ()
clear hostParameters host (ClapId paramId) clearFlags = do 
    funPtr <- peek $ p'clap_host_params'clear hostParameters
    mK'clear funPtr host (fromIntegral paramId) (fromIntegral $ flagsToInt clearFlags)

requestFlush :: HostParametersHandle -> HostHandle -> IO ()
requestFlush hostParameters host = do 
    funPtr <- peek $ p'clap_host_params'request_flush hostParameters
    mK'request_flush funPtr host