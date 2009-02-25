{-# LANGUAGE EmptyDataDecls, ForeignFunctionInterface #-}
module Data.Text.ICU.Conversion
    (
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import Data.ByteString (ByteString)
import Data.Text.Foreign
import Data.Text
import System.IO.Unsafe (unsafePerformIO)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.C.String (CString)
import Data.Int (Int32)
import Data.Word (Word8, Word16)

newtype Encoding = Encoding ByteString

data Converter

fromUnicode :: Ptr UConverter -> Text -> ByteString
fromUnicode cnv txt = unsafePerformIO $ do
  useAsPtr txt $ \ptr len -> do
    dstLen <- getMaxBytesForString len cnv
    B.createAndTrim dstLen $ \dst -> do
      alloca $ \errPtr -> do
        fromIntegral `fmap` fromUChars cnv dst (fromIntegral dstLen) ptr (fromIntegral len) errPtr

data UConverter

foreign import ccall unsafe "__get_max_bytes_for_string" getMaxBytesForString
    :: Int -> Ptr UConverter -> IO Int

type UChar = Word16
type UErrorCode = Int

foreign import ccall unsafe "ucnv_fromUChars" fromUChars
    :: Ptr UConverter -> Ptr Word8 -> Int32 -> Ptr UChar -> Int32
    -> Ptr UErrorCode -> IO Int32
