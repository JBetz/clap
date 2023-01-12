module Clap.Interface.Stream where

import Clap.Interface.Foreign.Stream
import Data.Int
import Data.Word
import Foreign.Ptr
import Foreign.Storable

newtype InputStreamHandle = InputStreamHandle { unInputStreamHandle :: Ptr C'clap_istream }
    deriving (Show)

newtype OutputStreamHandle = OutputStreamHandle { unOutputStreamHandle :: Ptr C'clap_ostream }
    deriving (Show)

read :: InputStreamHandle -> Ptr () -> Word64 -> IO Int64
read (InputStreamHandle inputStream) buffer size = do
    funPtr <- peek $ p'clap_istream'read inputStream
    pure $ fromIntegral $ mK'read funPtr inputStream buffer (fromIntegral size)

write :: OutputStreamHandle -> Ptr () -> Word64 -> IO Int64
write (OutputStreamHandle outputStream) buffer size = do
    funPtr <- peek $ p'clap_ostream'write outputStream
    pure $ fromIntegral $ mK'write funPtr outputStream buffer (fromIntegral size)