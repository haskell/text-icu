{-# LANGUAGE MultiParamTypeClasses,TypeSynonymInstances,FlexibleInstances #-}
-- |This module makes it easier to format numbers or dates/times based on a format specification.
-- E.g.:
--
-- >dt <- getNow
-- >format (dfsLong,dfsLong,"en_US",pack "GMT") dt
--
-- &#x21E8; @Right \"April 1, 2009 8:40:05 PM GMT+00:00\"@
--
-- >format (nfsSpellOut,"fr") (123.456::Double)
--
-- &#x21E8; @Right \"cent vingt-trois virgule quatre cinq six\"@
--
-- >format (nfsSpellOut,\"he\") (123456::Int)
--
-- &#x21E8; @Right \"&#1502;&#1488;&#1492; &#1506;&#1513;&#1512;&#1497;&#1501;&#1493;&#1513;&#1500;&#1493;&#1513;&#1492; &#1488;&#1500;&#1507; &#1488;&#1512;&#1489;&#1506; &#1502;&#1488;&#1493;&#1514; &#1495;&#1502;&#1497;&#1513;&#1497;&#1501; &#1493;&#1513;&#1513;\"@
module Data.Text.ICU.Formatting (Formattable(..)) where

import Data.Text
import Data.Text.ICU.Calendars
import Data.Text.ICU.Error
import Data.Text.ICU.NumberFormatting
import Data.Text.ICU.DateTimeFormatting

class Formattable formatSpec dat where
    format :: formatSpec -> dat -> Either ErrorCode Text

instance Formattable NumberFormat Int where
    format = formatIntegral

instance Formattable (NumberFormatStyle,String) Int where
    format (nfs,loc) x = case openNumberFormat nfs loc of
                           Right nf -> formatIntegral nf x
                           Left err -> Left err

instance Formattable NumberFormat Integer where
    format = formatIntegral

instance Formattable (NumberFormatStyle,String) Integer where
    format (nfs,loc) x = case openNumberFormat nfs loc of
                           Right nf -> formatIntegral nf x
                           Left err -> Left err

instance Formattable NumberFormat Double where
    format = formatRealFrac

instance Formattable (NumberFormatStyle,String) Double where
    format (nfs,loc) x = case openNumberFormat nfs loc of
                           Right nf -> formatRealFrac nf x
                           Left err -> Left err

instance Formattable NumberFormat Float where
    format = formatRealFrac

instance Formattable (NumberFormatStyle,String) Float where
    format (nfs,loc) x = case openNumberFormat nfs loc of
                           Right nf -> formatRealFrac nf x
                           Left err -> Left err

instance Formattable DateFormat Date where
    format = formatDate

instance Formattable (DateFormatStyle,DateFormatStyle,String,Text) Date where
    format (tfs,dfs,loc,tzID) dt = case openDateFormat tfs dfs loc tzID of
                                     Right df -> formatDate df dt
                                     Left err -> Left err

instance Formattable DateFormat Calendar where
    format df cal = case getMillis cal of
                      Right d -> formatDate df d
                      Left err -> Left err

instance Formattable (DateFormatStyle,DateFormatStyle,String,Text) Calendar where
    format (tfs,dfs,loc,tzID) cal = case openDateFormat tfs dfs loc tzID of
                                      Right df -> format df cal
                                      Left err -> Left err

