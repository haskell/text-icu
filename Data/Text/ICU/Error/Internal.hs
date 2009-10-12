module Data.Text.ICU.Error.Internal
    (
    -- * Functions
      isFailure
    , isSuccess
    , handleError
    , preflight
    , throwOnError
    , withError
    ) where

import Control.Exception (throw)
import Data.Int (Int32)
import Data.Text.ICU.Error.Codes (ErrorCode(..), UErrorCode,
                                  u_BUFFER_OVERFLOW_ERROR)
import Foreign.C.String (CString, peekCAStringLen)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray0)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek, poke)

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

withError :: (Ptr UErrorCode -> IO a) -> IO (ErrorCode, a)
{-# INLINE withError #-}
withError action = alloca $ \errPtr -> do
                     poke errPtr 0
                     ret <- action errPtr
                     err <- peek errPtr
                     return (ErrorCode err, ret)

handleError :: (Ptr UErrorCode -> IO a) -> IO a
{-# INLINE handleError #-}
handleError action = alloca $ \errPtr -> do
                       poke errPtr 0
                       ret <- action errPtr
                       throwOnError =<< peek errPtr
                       return ret

preflight :: Int -> (CString -> Int32 -> Ptr UErrorCode -> IO Int32)
          -> IO String
preflight capacity act =
    allocaArray0 capacity $ \buf -> do
      (err,l) <- withError $ act buf (fromIntegral capacity)
      if err == u_BUFFER_OVERFLOW_ERROR
          then preflight (capacity * 2) act
          else if isSuccess err
               then peekCAStringLen (buf, fromIntegral l)
               else throw err
