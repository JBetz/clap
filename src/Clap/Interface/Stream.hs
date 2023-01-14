module Clap.Interface.Stream where

import Clap.Interface.Foreign.Stream
import Data.Int
import Data.Word
import Foreign.Ptr
import Foreign.Storable

type InputStreamHandle = Ptr C'clap_istream

type OutputStreamHandle = Ptr C'clap_ostream

read :: InputStreamHandle -> Ptr () -> Word64 -> IO Int64
read inputStream buffer size = do
    funPtr <- peek $ p'clap_istream'read inputStream
    pure $ fromIntegral $ mK'read funPtr inputStream buffer (fromIntegral size)

write :: OutputStreamHandle -> Ptr () -> Word64 -> IO Int64
write outputStream buffer size = do
    funPtr <- peek $ p'clap_ostream'write outputStream
    pure $ fromIntegral $ mK'write funPtr outputStream buffer (fromIntegral size)