{-# LANGUAGE CPP, DeriveDataTypeable, ForeignFunctionInterface,
    GeneralizedNewtypeDeriving,EmptyDataDecls #-}
module Data.Text.ICU.FieldPosition (FieldPosition(..)) where

import Data.Int (Int32)
import Foreign.Storable (Storable(..))

#include <unicode/umisc.h>

data FieldPosition = FieldPosition {
      field :: Int32,
      beginIndex :: Int32,
      endIndex :: Int32}

instance Storable FieldPosition where
    sizeOf _ = (#size UFieldPosition)
    alignment _ = alignment (undefined :: Int32)
    peek ptr = do
        field' <- (#peek UFieldPosition,field) ptr
        beginIndex' <- (#peek UFieldPosition,beginIndex) ptr
        endIndex' <- (#peek UFieldPosition,endIndex) ptr
        return FieldPosition {field=field',beginIndex=beginIndex',endIndex=endIndex'}
    poke ptr (FieldPosition field' beginIndex' endIndex') =
        do
          (#poke UFieldPosition,field) ptr field'
          (#poke UFieldPosition,beginIndex) ptr beginIndex'
          (#poke UFieldPosition,endIndex) ptr endIndex'
