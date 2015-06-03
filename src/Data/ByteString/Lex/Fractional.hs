{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
----------------------------------------------------------------
--                                                    2015.06.02
-- |
-- Module      :  Data.ByteString.Lex.Fractional
-- Copyright   :  Copyright (c) 2015 wren gayle romano
-- License     :  BSD2
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  provisional
-- Portability :  Haskell98
--
-- Functions for parsing and producing 'Fractional' values from\/to
-- 'ByteString's based on the \"Char8\" encoding. That is, we assume
-- an ASCII-compatible encoding of alphanumeric characters.
--
-- /Since: 0.5.0/
----------------------------------------------------------------
module Data.ByteString.Lex.Fractional
    (
    -- * General combinators
      readSigned
    -- packSigned
    -- * Decimal conversions
    , readDecimal
    -- packDecimal
    -- TODO: asDecimal -- this will be really hard to make efficient...
    -- * Hexadecimal conversions
    , readHexadecimal
    -- packHexadecimal
    -- asHexadecimal
    -- * Octal conversions
    , readOctal
    -- packOctal
    -- asOctal -- this will be really hard to make efficient...
    -- * Exponential conversions
    , readExponential
    -- packExponential
    -- asExponential
    ) where

import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Unsafe   as BSU
import qualified Data.ByteString.Lex.Integral as I
import           Data.ByteString.Lex.Integral (readSigned)

----------------------------------------------------------------
----------------------------------------------------------------

-- | A helper function to ensure consistent strictness.
-- TODO: should we really be this strict?
justPair :: a -> b -> Maybe (a,b)
justPair x y
    | x `seq` y `seq` False = undefined
    | otherwise = Just (x,y)
{-# INLINE justPair #-}


----------------------------------------------------------------
----- Decimal

-- | Read an unsigned\/non-negative fractional value in ASCII decimal
-- format; that is, anything matching the regex @\d+(\.\d+)?@.
-- Returns @Nothing@ if there is no such number at the beginning
-- of the string, otherwise returns @Just@ the number read and the
-- remainder of the string.
readDecimal :: (Fractional a) => ByteString -> Maybe (a, ByteString)
{-# SPECIALIZE readDecimal ::
    ByteString -> Maybe (Float,    ByteString),
    ByteString -> Maybe (Double,   ByteString),
    ByteString -> Maybe (Rational, ByteString) #-}
readDecimal xs0 =
    case I.readDecimal xs0 of -- BUG: defaults to Integer...
    Nothing -> Nothing
    Just (whole, xs1)
        | BS.null xs1 || 0x2E /= BSU.unsafeHead xs1 ->
            justPair (fromIntegral whole) xs1
        | otherwise ->
            case I.readDecimal (BSU.unsafeTail xs1) of -- BUG: defaults to Integer...
            Nothing          -> justPair (fromIntegral whole) xs1
            Just (part, xs2) ->
                let base = 10 ^ (BS.length xs1 - 1 - BS.length xs2)
                    frac = fromIntegral whole + (fromIntegral part / base)
                in justPair frac xs2

----------------------------------------------------------------
-- If and only if(!) we have @Real a@, then we can use 'toRational'...

-- TODO:
-- Convert a non-negative fractional number into an (unsigned)
-- ASCII decimal string. Returns @Nothing@ on negative inputs.
-- packDecimal :: (Fractional a) => a -> Maybe ByteString


----------------------------------------------------------------
----------------------------------------------------------------
----- Hexadecimal

-- | Read a non-negative integral value in ASCII hexadecimal format.
-- Returns @Nothing@ if there is no integer at the beginning of the
-- string, otherwise returns @Just@ the integer read and the remainder
-- of the string.
--
-- This function does not recognize the various hexadecimal sigils
-- like \"0x\", but because there are so many different variants,
-- those are best handled by helper functions which then use this
-- function for the actual numerical parsing. This function recognizes
-- both upper-case, lower-case, and mixed-case hexadecimal.
readHexadecimal :: (Fractional a) => ByteString -> Maybe (a, ByteString)
{-# SPECIALIZE readHexadecimal ::
    ByteString -> Maybe (Float,    ByteString),
    ByteString -> Maybe (Double,   ByteString),
    ByteString -> Maybe (Rational, ByteString) #-}
readHexadecimal xs0 = 
    case I.readHexadecimal xs0 of -- BUG: defaults to Integer...
    Nothing       -> Nothing
    Just (n, xs1) -> justPair (fromIntegral n) xs1


-- TODO:
-- Convert a non-negative integer into a lower-case ASCII hexadecimal
-- string. Returns @Nothing@ on negative inputs.
-- packHexadecimal :: (Fractional a) => a -> Maybe ByteString


----------------------------------------------------------------
----------------------------------------------------------------
----- Octal

-- | Read a non-negative integral value in ASCII octal format.
-- Returns @Nothing@ if there is no integer at the beginning of the
-- string, otherwise returns @Just@ the integer read and the remainder
-- of the string.
--
-- This function does not recognize the various octal sigils like
-- \"0o\", but because there are different variants, those are best
-- handled by helper functions which then use this function for the
-- actual numerical parsing.
readOctal :: (Fractional a) => ByteString -> Maybe (a, ByteString)
{-# SPECIALIZE readOctal ::
    ByteString -> Maybe (Float,    ByteString),
    ByteString -> Maybe (Double,   ByteString),
    ByteString -> Maybe (Rational, ByteString) #-}
readOctal xs0 = 
    case I.readOctal xs0 of -- BUG: defaults to Integer...
    Nothing       -> Nothing
    Just (n, xs1) -> justPair (fromIntegral n) xs1

-- TODO:
-- Convert a non-negative integer into an ASCII octal string.
-- Returns @Nothing@ on negative inputs.
-- packOctal :: (Fractional a) => a -> Maybe ByteString


----------------------------------------------------------------
----------------------------------------------------------------
----- Exponential

-- | Read an unsigned\/non-negative fractional value in ASCII
-- exponential format; that is, anything matching the regex
-- @\d+(\.\d+)?([eE][\+\-]?\d+)?@. Returns @Nothing@ if there is
-- no such number at the beginning of the string, otherwise returns
-- @Just@ the number read and the remainder of the string.
readExponential :: (Fractional a) => ByteString -> Maybe (a, ByteString)
{-# SPECIALIZE readExponential ::
    ByteString -> Maybe (Float,    ByteString),
    ByteString -> Maybe (Double,   ByteString),
    ByteString -> Maybe (Rational, ByteString) #-}
readExponential xs0 =
    case readDecimal xs0 of
    Nothing -> Nothing
    Just (f, xs1)
        | BS.null xs1 || (0x65 /= BSU.unsafeHead xs1 && 0x45 /= BSU.unsafeHead xs1) ->
            justPair f xs1
        | otherwise ->
            -- TODO: benchmark the benefit of inlining 'readSigned' here
            case readSigned I.readDecimal (BSU.unsafeTail xs1) of -- BUG: defaults to Integer...
            Nothing       -> justPair f xs1
            Just (e, xs2) ->
                let f' = if e >= 0 then f * (10 ^ e) else f / (10 ^ abs e)
                in justPair f' xs2

----------------------------------------------------------------
----------------------------------------------------------- fin.