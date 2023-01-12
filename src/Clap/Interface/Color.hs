module Clap.Interface.Color where

import Data.Word

data Color = Color
    { color_alpha :: Word8
    , color_red :: Word8
    , color_green :: Word8
    , color_blue :: Word8
    } deriving (Show)