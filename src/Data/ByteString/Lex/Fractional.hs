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
    , readDecimalLimited
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

import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Unsafe       as BSU
import qualified Data.ByteString.Lex.Integral as I
import           Data.ByteString.Lex.Integral (readSigned)

----------------------------------------------------------------
----------------------------------------------------------------

-- | A helper function to ensure consistent strictness.
-- TODO: should we really be this strict?
justPair :: a -> b -> Maybe (a,b)
{-# INLINE justPair #-}
justPair x y
    | x `seq` y `seq` False = undefined
    | otherwise = Just (x,y)

pair :: a -> b -> (a,b)
{-# INLINE pair #-}
pair x y
    | x `seq` y `seq` False = undefined
    | otherwise = (x,y)


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
        | BS.null xs'                -> justPair (fromInteger whole) BS.empty
        | 0x2E /= BSU.unsafeHead xs' -> justPair (fromInteger whole) xs'
        | otherwise ->
            case I.readDecimal (BSU.unsafeTail xs') of
            Nothing           -> justPair (fromInteger whole) xs'
            Just (part, xs'') ->
                -- TODO: it'd be more robust(?) but slower to use: @(whole*base + fromInteger part) / base@
                let base = 10 ^ (BS.length xs' - 1 - BS.length xs'')
                    frac = fromInteger whole + (fromInteger part / base)
                in justPair frac xs''


-- | A representation of unsigned fractional numbers decomposed into a significand\/mantissa and a decimal exponent. This allows efficient scaling by decimal exponents, as:
--
-- > (DF f e1) * 10 ^^ e2 = DF f (e1 + e2)
--
-- This is a helper type for documenting the result of 'readDecimalLimited'.
--
-- HACK: the first component should be some @a@-specific intermediate representation, as defined by a fundep or typefamily! Using 'Word64' is sufficient for 'Float' and 'Double', but can break elsewhere!
data DecimalFraction a = DF {-# UNPACK #-}!Word64 {-# UNPACK #-}!Int

-- | Extract the fractional number encoded in the record.
--
-- > fromDF (DF frac scale) = fromIntegral frac * (10 ^^ scale)
fromDF :: Fractional a => DecimalFraction a -> a
fromDF (DF frac scale) = fromIntegral frac * (10 ^^ scale)

-- | A helpful monomorphic constructor.
integerDF :: Integer -> DecimalFraction a
integerDF whole = DF (fromInteger whole) 0

-- | A variant of 'readDecimal' which only reads up to some limited precision. The first argument gives the number of decimal digits at which to limit the precision.
--
-- For types with inherently limited precision (e.g., 'Float' and 'Double'), this is far more efficient than 'readDecimal' when the input contains more precision than the type can store. For all 'RealFloat' types you can use the following definition to access the inherent limitations of the type:
--
-- > limit :: RealFloat a => a -> Int
-- > limit proxy = length (show (floatRadix proxy ^ floatDigits proxy))
--
-- N.B., passing a precision threshold greater than the inherent limit will generally degrade precision. This is because doing so causes the function to perform excess manipulations of the intermediate values which don't quite come out to being the identity (like they should).
--
-- Even if the result type has unlimited precision (e.g., 'Rational') this may be far more efficient than 'readDecimal' (it is for 'Rational', in fact). The reason being that it delays the scaling the significand\/mantissa by the exponent, thus allowing you to further adjust the exponent before computing the final value (e.g., as in 'readExponentialLimited'). This avoids the need to renormalize intermediate results, and allows faster computation of the scaling factor by doing it all at once.
readDecimalLimited :: forall a. (Fractional a) => Int -> ByteString -> Maybe (DecimalFraction a, ByteString)
{-# SPECIALIZE readDecimalLimited ::
    ByteString -> Maybe (DecimalFraction Float,    ByteString),
    ByteString -> Maybe (DecimalFraction Double,   ByteString),
    ByteString -> Maybe (DecimalFraction Rational, ByteString) #-}
readDecimalLimited p = start
    where
    -- BUG: need to deal with leading zeros re 'magicLength'
    start :: ByteString -> Maybe (DecimalFraction a, ByteString)
    start xs
        | BS.length xs <= p = readDecimal xs
        | otherwise         =
            -- HACK: monomorphizing at 'Word64'
            case BSLex.readDecimal (BS.take p xs) of
            Nothing          -> Nothing
            Just (frac, ys)
                | BS.null ys ->
                    let scale = BS.length
                              . BS.takeWhile isDecimal
                              $ BS.drop p xs
                    in Just $! dropDecimalPart frac scale
                        (BS.drop (p+scale) xs)
                | otherwise  ->
                    let len = BS.length ys in
                    Just $! readDecimalPart frac len
                        (BS.drop (p-len) xs)

    dropDecimalPart :: Word64 -> Int -> ByteString -> (DecimalFraction a, ByteString)
    dropDecimalPart frac scale xs
        | frac `seq` scale `seq` False = undefined
        | BS.null xs = pair (DF frac scale) BS.empty
        | otherwise  = pair (DF frac scale) $!
            if isNotPeriod (BSU.unsafeHead xs)
            then xs
            else BS.dropWhile isDecimal (BSU.unsafeTail xs)
    
    readDecimalPart :: Word64 -> Int -> ByteString -> (DecimalFraction a, ByteString)
    readDecimalPart frac len xs
        | frac `seq` len `seq` False      = undefined
        | BS.null xs                      =
            error _readDecimalLimited_readDecimalPart_impossible
        | isNotPeriod (BSU.unsafeHead xs) = pair (DF frac 0) xs
        | otherwise                       =
            let ys = BS.take len (BSU.unsafeTail xs) in
            case BSLex.readDecimal ys of
            Nothing           -> pair (DF frac 0) xs
            Just (part, ys')
                | BS.null ys' ->
                    let scale = BS.length ys in
                    pair (DF (frac * (10 ^ scale) + part) (negate scale))
                        (BS.dropWhile isDecimal (BS.drop (1+scale) xs))
                | otherwise   ->
                    let scale = BS.length ys - BS.length ys' in
                    pair (DF (frac * (10 ^ scale) + part) (negate scale))
                        (BS.drop (1+scale) xs)


_readDecimalLimited_readDecimalPart_impossible :: String
_readDecimalLimited_readDecimalPart_impossible =
    "readDecimalLimited: the impossible happened when trying to read the decimal part"

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
--
-- N.B., the current implementation assumes the exponent is small
-- enough to fit into an 'Int'. This gives a significant performance
-- increase for @a ~ Float@ and @a ~ Double@ and agrees with the
-- 'RealFloat' class which has 'exponent' returning an 'Int'. If
-- you need a larger exponent, contact the maintainer.
--
-- N.B., if your fractional type has limited precision (like 'Float' and 'Double' do) and you expect to be reading in strings with excessive precision, then you should check out the version in "Data.ByteString.Lex.RealFloat" which 
readExponential :: (Fractional a) => ByteString -> Maybe (a, ByteString)
{-# SPECIALIZE readExponential ::
    ByteString -> Maybe (Float,    ByteString),
    ByteString -> Maybe (Double,   ByteString),
    ByteString -> Maybe (Rational, ByteString) #-}
readExponential xs =
    -- TODO: inlining 'readDecimal' here would allow a couple
    -- branches to shortcut. But our benchmark "BenchReadExponential"
    -- (which mainly tests the final branch, which doesn't
    -- short-circuit) indicates comparable or marginally worse
    -- performance (compare @readExponential11@ vs @readExponential3@).
    -- We need to investigate this more fully for realistic inputs.
    case readDecimal xs of
    Nothing -> Nothing
    Just (frac, xs')
        | BS.null xs'                 -> justPair frac BS.empty
        | isNotE (BSU.unsafeHead xs') -> justPair frac xs'
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