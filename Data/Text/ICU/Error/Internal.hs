{-# LANGUAGE DeriveDataTypeable, ForeignFunctionInterface #-}

module Data.Text.ICU.Error.Internal
    (
    -- * Types
      ErrorCode(..)
    -- ** Low-level types
    , UErrorCode
    -- * Functions
    , isFailure
    , isSuccess
    , errorName
    , handleError
    , throwOnError
    ) where

import Control.Exception
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Data.Typeable (Typeable)
import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CInt)
import Foreign.Storable
import System.IO.Unsafe (unsafePerformIO)

type UErrorCode = CInt

-- | ICU error code.
newtype ErrorCode = ErrorCode {
      fromErrorCode :: UErrorCode
    } deriving (Eq, Typeable)

instance Show ErrorCode where
    show code = "ErrorCode " ++ errorName code

instance Exception ErrorCode

-- | Indicate whether the given error code is a success.
isSuccess :: ErrorCode -> Bool
{-# INLINE isSuccess #-}
isSuccess = (<= 0) . fromErrorCode

-- | Indicate whether the given error code is a failure.
isFailure :: ErrorCode -> Bool
{-# INLINE isFailure #-}
isFailure = (> 0) . fromErrorCode

-- | Throw an exception if the given code is actually an error.
throwOnError :: UErrorCode -> IO ()
{-# INLINE throwOnError #-}
throwOnError code = do
  let err = (ErrorCode code)
  if isFailure err
    then throw err
    else return ()

handleError :: (Ptr UErrorCode -> IO a) -> IO a
{-# INLINE handleError #-}
handleError action = alloca $ \errPtr -> do
                       poke errPtr 0
                       ret <- action errPtr
                       throwOnError =<< peek errPtr
                       return ret

-- | Return a string representing the name of the given error code.
errorName :: ErrorCode -> String
errorName code = unsafePerformIO $
                 peekCString (u_errorName (fromErrorCode code))

foreign import ccall unsafe "unicode/utypes.h u_errorName_4_0" u_errorName
    :: UErrorCode -> CString
