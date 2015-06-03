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


-- NOTE: We use 'fromInteger' everywhere instead of 'fromIntegral' in order to fix the types of the calls to 'I.readDecimal', etc. This is always correct, but for some result types there are other intermediate types which may be faster.

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
readDecimal xs =
    case I.readDecimal xs of
    Nothing -> Nothing
    Just (whole, xs')
        | BS.null xs' || 0x2E /= BSU.unsafeHead xs' ->
            justPair (fromInteger whole) xs'
        | otherwise ->
            case I.readDecimal (BSU.unsafeTail xs') of
            Nothing          -> justPair (fromInteger whole) xs'
            Just (part, xs'') ->
                -- TODO: it'd be more robust (but slower?) to use: @(whole*base + fromInteger part) / base@
                let base = 10 ^ (BS.length xs' - 1 - BS.length xs'')
                    frac = fromInteger whole + (fromInteger part / base)
                in justPair frac xs''

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
readHexadecimal xs = 
    case I.readHexadecimal xs of
    Nothing       -> Nothing
    Just (n, xs') -> justPair (fromInteger n) xs'


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
readOctal xs = 
    case I.readOctal xs of
    Nothing       -> Nothing
    Just (n, xs') -> justPair (fromInteger n) xs'

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
readExponential xs =
    -- TODO: inlining 'readDecimal' here would allow a couple
    -- branches to shortcut. Should be a slight improvement for
    -- those cases, but seems to give marginally worse performance
    -- for the final branch (instruction caching?)
    case readDecimal xs of
    Nothing -> Nothing
    Just (frac, xs')
        | BS.null xs' || isNotE (BSU.unsafeHead xs') ->
            justPair frac xs'
        | otherwise ->
            -- According to 'RealFrac' exponents should be 'Int',
            -- so using that to avoid defaulting here. N.B., this
            -- gives a major performance bost over using 'Integer'
            -- for @a~Float@ and @a~Double@.
            case readSigned I.readDecimal (BSU.unsafeTail xs') of
            Nothing        -> justPair frac xs'
            Just (e, xs'') -> justPair (frac * (10 ^^ (e::Int)) xs''
    where
    {-# INLINE isNotE #-}
    isNotE w = 0x65 /= w && 0x45 /= w

----------------------------------------------------------------
----------------------------------------------------------- fin.