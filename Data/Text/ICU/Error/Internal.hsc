{-# LANGUAGE DeriveDataTypeable, ForeignFunctionInterface,
    ScopedTypeVariables #-}

module Data.Text.ICU.Error.Internal
    (
    -- * Types
      ICUError(..)
    -- ** Low-level types
    , UErrorCode
    , ParseError(errError, errLine, errOffset)
    , UParseError
    -- * Functions
    , isFailure
    , isSuccess
    , errorName
    , handleError
    , handleOverflowError
    , handleParseError
    , throwOnError
    , withError
    ) where

import Control.Exception (Exception, throwIO)
import Data.Function (fix)
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)
import Foreign.Marshal.Array (allocaArray)
import Data.Int (Int32)
import Data.Typeable (Typeable)
import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CInt(..))
import Foreign.Storable (Storable(..))
import System.IO.Unsafe (unsafePerformIO)

#include <unicode/parseerr.h>
#include <unicode/utypes.h>

type UErrorCode = CInt

-- | ICU error type.  This is an instance of the 'Exception' type
-- class.  A value of this type may be thrown as an exception by most
-- ICU functions.
newtype ICUError = ICUError {
      fromErrorCode :: UErrorCode
    } deriving (Eq, Typeable)

instance Show ICUError where
    show code = "ICUError " ++ errorName code

instance Exception ICUError

-- | Detailed information about parsing errors.  Used by ICU parsing
-- engines that parse long rules, patterns, or programs, where the
-- text being parsed is long enough that more information than an
-- 'ICUError' is needed to localize the error.
data ParseError = ParseError {
      errError :: ICUError
    , errLine :: !(Maybe Int)
    -- ^ The line on which the error occured.  If the parser uses this
    -- field, it sets it to the line number of the source text line on
    -- which the error appears, which will be be a positive value.  If
    -- the parser does not support line numbers, the value will be
    -- 'Nothing'.
    , errOffset :: !(Maybe Int)
    -- ^ The character offset to the error.  If the 'errLine' field is
    -- 'Just' some value, then this field contains the offset from the
    -- beginning of the line that contains the error.  Otherwise, it
    -- represents the offset from the start of the text.  If the
    -- parser does not support this field, it will have a value of
    -- 'Nothing'.
    } deriving (Show, Typeable)

type UParseError = ParseError

instance Exception ParseError

instance Storable ParseError where
    sizeOf _    = #{size UParseError}
    alignment _ = alignment (undefined :: CString)
    peek ptr = do
      (line::Int32) <- #{peek UParseError, line} ptr
      (offset::Int32) <- #{peek UParseError, offset} ptr
      let wrap k = if k == -1 then Nothing else Just $! fromIntegral k
      return $! ParseError undefined (wrap line) (wrap offset)

-- | Indicate whether the given error code is a success.
isSuccess :: ICUError -> Bool
{-# INLINE isSuccess #-}
isSuccess = (<= 0) . fromErrorCode

-- | Indicate whether the given error code is a failure.
isFailure :: ICUError -> Bool
{-# INLINE isFailure #-}
isFailure = (> 0) . fromErrorCode

-- | Throw an exception if the given code is actually an error.
throwOnError :: UErrorCode -> IO ()
{-# INLINE throwOnError #-}
throwOnError code = do
  let err = (ICUError code)
  if isFailure err
    then throwIO err
    else return ()

withError :: (Ptr UErrorCode -> IO a) -> IO (ICUError, a)
{-# INLINE withError #-}
withError action = with 0 $ \errPtr -> do
                     ret <- action errPtr
                     err <- peek errPtr
                     return (ICUError err, ret)

handleError :: (Ptr UErrorCode -> IO a) -> IO a
{-# INLINE handleError #-}
handleError action = with 0 $ \errPtr -> do
                       ret <- action errPtr
                       throwOnError =<< peek errPtr
                       return ret

-- | Deal with ICU functions that report a buffer overflow error if we
-- give them an insufficiently large buffer.  Our first call will
-- report a buffer overflow, in which case we allocate a correctly
-- sized buffer and try again.
handleOverflowError :: (Storable a) =>
                       Int
                    -- ^ Initial guess at buffer size.
                    -> (Ptr a -> Int32 -> Ptr UErrorCode -> IO Int32)
                    -- ^ Function that retrieves data.
                    -> (Ptr a -> Int -> IO b)
                    -- ^ Function that fills destination buffer if no
                    -- overflow occurred.
                    -> IO b
handleOverflowError guess fill retrieve =
  alloca $ \uerrPtr -> flip fix guess $ \loop n ->
    (either (loop . fromIntegral) return =<<) . allocaArray n $ \ptr -> do
      poke uerrPtr 0
      ret <- fill ptr (fromIntegral n) uerrPtr
      err <- peek uerrPtr
      case undefined of
        _| err == (#const U_BUFFER_OVERFLOW_ERROR)
                     -> return (Left ret)
         | err > 0   -> throwIO (ICUError err)
         | otherwise -> Right `fmap` retrieve ptr (fromIntegral ret)

handleParseError :: (ICUError -> Bool)
                 -> (Ptr UParseError -> Ptr UErrorCode -> IO a) -> IO a
handleParseError isParseError action = with 0 $ \uerrPtr ->
  alloca $ \perrPtr -> do
    ret <- action perrPtr uerrPtr
    err <- ICUError `fmap` peek uerrPtr
    case undefined of
     _| isParseError err -> do
                         perr <- peek perrPtr
                         throwIO perr { errError = err }
      | isFailure err -> throwIO err
      | otherwise     -> return ret

-- | Return a string representing the name of the given error code.
errorName :: ICUError -> String
errorName code = unsafePerformIO $
                 peekCString (u_errorName (fromErrorCode code))

foreign import ccall unsafe "hs_text_icu.h __hs_u_errorName" u_errorName
    :: UErrorCode -> CString
