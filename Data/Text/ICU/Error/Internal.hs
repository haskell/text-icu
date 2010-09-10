{-# LANGUAGE DeriveDataTypeable, ForeignFunctionInterface #-}

module Data.Text.ICU.Error.Internal
    (
    -- * Types
      ICUError(..)
    -- ** Low-level types
    , UErrorCode
    -- * Functions
    , isFailure
    , isSuccess
    , errorName
    , handleError
    , throwOnError
    , withError
    ) where

import Control.Exception (Exception, throw)
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Utils (with)
import Data.Typeable (Typeable)
import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CInt)
import Foreign.Storable (peek)
import System.IO.Unsafe (unsafePerformIO)

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
    then throw err
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

-- | Return a string representing the name of the given error code.
errorName :: ICUError -> String
errorName code = unsafePerformIO $
                 peekCString (u_errorName (fromErrorCode code))

foreign import ccall unsafe "hs_text_icu.h __hs_u_errorName" u_errorName
    :: UErrorCode -> CString
