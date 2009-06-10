{-# LANGUAGE CPP, DeriveDataTypeable, ForeignFunctionInterface,
    GeneralizedNewtypeDeriving,EmptyDataDecls #-}
module Data.Text.ICU.NumberFormatting
    (NumberFormatStyle(..),
     nfsPatternDecimal,nfsDecimal,nfsCurrency,nfsPercent,nfsScientific,
     nfsSpellOut,nfsOrdinal,nfsDuration,nfsPatternRuleBased,
     parseContextLen,
     defaultNumberFormatStyle,ignoreNumberFormatStyle,
     NumberFormat,openNumberFormat,formatIntegral,formatRealFrac,)
    where

#include <unicode/unum.h>

import Control.Exception (throw)
import Control.Monad (when)
import Data.Text (Text)
import Data.Text.Foreign (fromPtr)
import Data.Text.ICU.Error.Internal (ErrorCode,UErrorCode,isFailure,withError)
import Data.Text.ICU.Internal (UChar)
import Data.Typeable (Typeable)
import Data.Int (Int32)
import Data.Word (Word32,Word16)
import Foreign.C.String (CString,withCAString)
import Foreign.C.Types (CDouble)
import Foreign.ForeignPtr (ForeignPtr,newForeignPtr,withForeignPtr)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr (Ptr,nullPtr,FunPtr)
import Foreign.Storable (Storable(..))
import System.IO.Unsafe (unsafePerformIO)

newtype NumberFormatStyle = NumberFormatStyle {
      fromNumberFormatStyle :: Word32
    } deriving (Eq,Typeable,Show)

#{enum NumberFormatStyle,NumberFormatStyle,
  nfsPatternDecimal = UNUM_PATTERN_DECIMAL,
  nfsDecimal = UNUM_DECIMAL,
  nfsCurrency = UNUM_CURRENCY,
  nfsPercent = UNUM_PERCENT,
  nfsScientific = UNUM_SCIENTIFIC,
  nfsSpellOut = UNUM_SPELLOUT,
  nfsOrdinal = UNUM_ORDINAL,
  nfsDuration = UNUM_DURATION,
  nfsPatternRuleBased = UNUM_PATTERN_RULEBASED}

defaultNumberFormatStyle :: NumberFormatStyle
defaultNumberFormatStyle = NumberFormatStyle #const UNUM_DECIMAL

ignoreNumberFormatStyle :: NumberFormatStyle
ignoreNumberFormatStyle = NumberFormatStyle #const UNUM_PATTERN_DECIMAL

data ParseError = ParseError {
      line :: Int32,
      offset :: Int32,
      preContext :: Ptr Word16,
      postContext :: Ptr Word16}

parseContextLen :: Int32
parseContextLen = #const U_PARSE_CONTEXT_LEN

instance Storable ParseError where
    sizeOf _ = (#size UParseError)
    alignment _ = alignment (undefined :: Int32)
    peek ptr = do
        line' <- (#peek UParseError,line) ptr
        offset' <- (#peek UParseError,offset) ptr
        preContext' <- (#peek UParseError,preContext) ptr
        postContext' <- (#peek UParseError,postContext) ptr
        return ParseError {line=line',
                           offset=offset',
                           preContext=preContext',
                           postContext=postContext'}
    poke ptr (ParseError line' offset' preContext' postContext') =
        do
          (#poke UParseError,line) ptr line'
          (#poke UParseError,offset) ptr offset'
          (#poke UParseError,preContext) ptr preContext'
          (#poke UParseError,postContext) ptr postContext'

data UNumberFormat
type NumberFormat = ForeignPtr UNumberFormat

openNumberFormat :: NumberFormatStyle -> String -> NumberFormat
openNumberFormat style locale = unsafePerformIO $ do
  withCAString locale $ \locale' -> do
    (err,nf) <- withError $ unum_open (fromNumberFormatStyle style) nullPtr 0 locale' nullPtr
    when (isFailure err) (throw err)
    newForeignPtr unum_close nf

formatIntegral :: (Integral a) => NumberFormat -> a -> Text
formatIntegral nf x = unsafePerformIO $ do
  allocaArray 256 $ \dptr -> do
    withForeignPtr nf $ \pnf -> do
      (err,l) <- withError $ unum_format pnf (fromIntegral x) dptr 256 nullPtr
      when (isFailure err) (throw err)
      fromPtr dptr (fromIntegral l)

formatRealFrac :: (RealFrac a) => NumberFormat -> a -> Text
formatRealFrac nf x = unsafePerformIO $ do
  allocaArray 256 $ \dptr -> do
    withForeignPtr nf $ \pnf -> do
      (err,l) <- withError $ unum_formatDouble pnf (fromRational (toRational x)) dptr 256 nullPtr
      when (isFailure err) (throw err)
      fromPtr dptr (fromIntegral l)

foreign import ccall unsafe "unicode/unum.h unum_open_4_0" unum_open
    :: Word32 -> Ptr UChar -> Int32 -> CString -> Ptr ParseError -> Ptr UErrorCode -> IO (Ptr UNumberFormat)
foreign import ccall "unicode/unum.h &unum_close_4_0" unum_close
    :: FunPtr (Ptr UNumberFormat -> IO ())
foreign import ccall unsafe "unicode/unum.h unum_format_4_0" unum_format
    :: Ptr UNumberFormat -> Int32 -> Ptr UChar -> Int32 -> Ptr () -> Ptr UErrorCode -> IO Int32
foreign import ccall unsafe "unicode/unum.h unum_formatDouble_4_0" unum_formatDouble
    :: Ptr UNumberFormat -> CDouble -> Ptr UChar -> Int32 -> Ptr () -> Ptr UErrorCode -> IO Int32
