{-# LANGUAGE CPP,ForeignFunctionInterface,EmptyDataDecls #-}
module Data.Text.ICU.Enumeration
    (Enumeration,UEnumeration,
     enumerationCount,enumerationNext,enumerationUNext,enumerationReset,
     enumerationFinalizer,
     enumerationStrings,enumerationTexts)
    where

#include <unicode/uenum.h>

import Control.Monad (replicateM)
import Data.Maybe (Maybe)
import Data.Text (Text)
import Data.Text.Foreign (fromPtr)
import Data.Text.ICU.Error.Internal (ErrorCode,UErrorCode,isSuccess,withError)
import Data.Text.ICU.Internal (UChar)
import Data.Int (Int32)
import Foreign.C.String (CString,peekCAStringLen)
import Foreign.ForeignPtr (ForeignPtr,newForeignPtr,withForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr,FunPtr)
import Foreign.Storable (peek)
import System.IO.Unsafe (unsafePerformIO)

data UEnumeration
type Enumeration = ForeignPtr UEnumeration

enumerationFinalizer :: Ptr UEnumeration -> IO Enumeration
enumerationFinalizer = newForeignPtr uenum_close

enumerationReset :: Enumeration -> IO (Maybe ErrorCode)
enumerationReset e = do
  withForeignPtr e (\e' -> do
    (err,_) <- withError $ uenum_reset e'
    if isSuccess err
       then return Nothing
       else return $ Just err)

enumerationCount :: Enumeration -> IO (Either ErrorCode Int32)
enumerationCount e = do
  withForeignPtr e $ (\e' -> do
    (err,n) <- withError $ uenum_count e'
    if isSuccess err
       then return $ Right n
       else return $ Left err)

enumerationNext :: Enumeration -> IO (Either ErrorCode String)
enumerationNext e = do
  withForeignPtr e $ (\e' -> do
    alloca $ (\pn -> do
      (err,s) <- withError $ uenum_next e' pn
      if isSuccess err
         then do
           n <- peek pn
           s' <- peekCAStringLen (s,fromIntegral n)
           return $ Right s'
         else return $ Left err))

enumerationUNext :: Enumeration -> IO (Either ErrorCode Text)
enumerationUNext e = do
  withForeignPtr e $ (\e' -> do
    alloca $ (\pn -> do
      (err,t) <- withError $ uenum_unext e' pn
      if isSuccess err
         then do
           n <- peek pn
           t' <- fromPtr t (fromIntegral n)
           return $ Right t'
         else return $ Left err))

enumerationStrings :: Enumeration -> Either ErrorCode [String]
enumerationStrings e = unsafePerformIO $ do
                         en <- enumerationCount e
                         case en of
                           Right n -> do
                                   ss <- replicateM (fromIntegral n) (enumerationNext e)
                                   return $ alright ss
                           Left err -> return $ Left err

enumerationTexts :: Enumeration -> Either ErrorCode [Text]
enumerationTexts e = unsafePerformIO $ do
                       en <- enumerationCount e
                       case en of
                         Right n -> do
                                 ts <- replicateM (fromIntegral n) (enumerationUNext e)
                                 return $ alright ts
                         Left err -> return $ Left err

alright :: [Either l r] -> Either l [r]
alright es = if any isLeft es
             then Left (fromLeft (head (filter isLeft es)))
             else Right (map fromRight es)

isLeft :: Either l r -> Bool
isLeft (Left _) = True
isLeft _ = False

fromLeft :: Either l r -> l
fromLeft (Left l) = l
fromLeft (Right _) = error "Argument of fromLeft is Right."

fromRight :: Either l r -> r
fromRight (Right r) = r
fromRight (Left _) = error "Argument of fromRight is Left."

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
