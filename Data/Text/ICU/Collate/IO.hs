{-# LANGUAGE CPP, ForeignFunctionInterface #-}
-- |
-- Module      : Data.Text.ICU.Collate.IO
-- Copyright   : (c) 2010 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- String collation functions for Unicode, implemented as bindings to
-- the International Components for Unicode (ICU) libraries.

module Data.Text.ICU.Collate.IO
    (
    -- * Unicode collation API
    -- $api
      MCollator
    , open
    , collate
    ) where

import Data.Int (Int32)
import Data.Text (Text)
import Data.Text.Foreign (useAsPtr)
import Data.Text.ICU.Collate.Internal (MCollator, UCollator, withCollator, wrap)
import Data.Text.ICU.Error.Internal (UErrorCode, handleError)
import Data.Text.ICU.Internal (UChar, asOrdering)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt)
import Foreign.Ptr (Ptr, nullPtr)

-- $api
--

type UCollationResult = CInt

-- | Open a 'Collator' for comparing strings.
--
-- * If 'Nothing' is passed for the locale, the default locale
--   collation rules will be used.
--
-- * If ('Just' @\"\"@) or 'Just' @\"root\"@ is passed, UCA rules will
--   be used.
open :: Maybe String
     -- ^ The locale containing the required collation rules.
     -> IO MCollator
open loc = wrap =<< withName loc (handleError . ucol_open)
 where
   withName Nothing act = act nullPtr
   withName (Just n) act = withCString n act

-- | Compare two strings.
collate :: MCollator -> Text -> Text -> IO Ordering
collate c a b =
  withCollator c $ \cptr ->
    useAsPtr a $ \aptr alen ->
      useAsPtr b $ \bptr blen ->
        fmap asOrdering . handleError $
        ucol_strcoll cptr aptr (fromIntegral alen) bptr (fromIntegral blen)

foreign import ccall unsafe "hs_text_icu.h __hs_ucol_open" ucol_open
    :: CString -> Ptr UErrorCode -> IO (Ptr UCollator)

foreign import ccall unsafe "hs_text_icu.h __hs_ucol_strcoll" ucol_strcoll
    :: Ptr UCollator -> Ptr UChar -> Int32 -> Ptr UChar -> Int32
    -> Ptr UErrorCode -> IO UCollationResult
