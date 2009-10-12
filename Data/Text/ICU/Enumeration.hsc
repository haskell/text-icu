{-# LANGUAGE CPP,ForeignFunctionInterface,EmptyDataDecls #-}
module Data.Text.ICU.Enumeration
    (Enumeration,UEnumeration,
     enumerationCount,enumerationNext,enumerationUNext,enumerationReset,
     enumerationFinalizer,
     enumerationStrings,enumerationTexts)
    where

#include <unicode/uenum.h>

import Control.Exception (throw)
import Control.Monad (when,replicateM)
import Data.Text (Text)
import Data.Text.Foreign (fromPtr)
import Data.Text.ICU.Error.Codes (UErrorCode)
import Data.Text.ICU.Error.Internal (isFailure, withError)
import Data.Text.ICU.Internal (UChar)
import Data.Int (Int32)
import Foreign.C.String (CString,peekCAStringLen)
import Foreign.ForeignPtr (ForeignPtr,newForeignPtr,withForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr,FunPtr)
import Foreign.Storable (peek)

data UEnumeration
type Enumeration = ForeignPtr UEnumeration

enumerationFinalizer :: Ptr UEnumeration -> IO Enumeration
enumerationFinalizer = newForeignPtr uenum_close

enumerationReset :: Enumeration -> IO ()
enumerationReset e = do
  withForeignPtr e $ \e' -> do
    (err,_) <- withError $ uenum_reset e'
    when (isFailure err) (throw err)

enumerationCount :: Enumeration -> IO Int32
enumerationCount e = do
  withForeignPtr e $ \e' -> do
    (err,n) <- withError $ uenum_count e'
    when (isFailure err) (throw err)
    return n

enumerationNext :: Enumeration -> IO String
enumerationNext e = do
  withForeignPtr e $ \e' -> do
    alloca $ \pn -> do
      (err,s) <- withError $ uenum_next e' pn
      when (isFailure err) (throw err)
      n <- peek pn
      peekCAStringLen (s,fromIntegral n)

enumerationUNext :: Enumeration -> IO Text
enumerationUNext e = do
  withForeignPtr e $ \e' -> do
    alloca $ \pn -> do
      (err,t) <- withError $ uenum_unext e' pn
      when (isFailure err) (throw err)
      n <- peek pn
      fromPtr t (fromIntegral n)

enumerationStrings :: Enumeration -> IO [String]
enumerationStrings e = do
  enumerationReset e
  n <- enumerationCount e
  replicateM (fromIntegral n) (enumerationNext e)

enumerationTexts :: Enumeration -> IO [Text]
enumerationTexts e = do
  enumerationReset e
  n <- enumerationCount e
  replicateM (fromIntegral n) (enumerationUNext e)

foreign import ccall "unicode/uenum.h &uenum_close_4_0" uenum_close
    :: FunPtr (Ptr UEnumeration -> IO ())
foreign import ccall unsafe "unicode/uenum.h uenum_reset_4_0" uenum_reset
    :: Ptr UEnumeration -> Ptr UErrorCode -> IO ()
foreign import ccall unsafe "unicode/uenum.h uenum_count_4_0" uenum_count
    :: Ptr UEnumeration -> Ptr UErrorCode -> IO Int32
foreign import ccall unsafe "unicode/uenum.h uenum_next_4_0" uenum_next
    :: Ptr UEnumeration -> Ptr Int32 -> Ptr UErrorCode -> IO CString
foreign import ccall unsafe "unicode/uenum.h uenum_unext_4_0" uenum_unext
    :: Ptr UEnumeration -> Ptr Int32 -> Ptr UErrorCode -> IO (Ptr UChar)
