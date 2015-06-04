{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# LANGUAGE ScopedTypeVariables #-}
----------------------------------------------------------------
--                                                    2015.06.04
-- |
-- Module      :  BenchReadExponential.NewImplementations
-- Copyright   :  Copyright (c) 2015 wren gayle romano,
--                              2015 Hirotomo Moriwaki
-- License     :  BSD2
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  benchmark
-- Portability :  ScopedTypeVariables, MPTCs, FunDeps
--
-- Benchmark the speed of parsing floating point numbers. This
-- benchmark originally came from @bytestring-read@ version 0.3.0.
----------------------------------------------------------------
module BenchReadExponential.NewImplementations
    ( readExponential1
    , readExponential11
    , readExponential2
    , readExponential3
    , readExponential31
    , readExponential32
    , readExponential4
    , readExponential41
    ) where

import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Unsafe       as BSU
import           Data.Word                    (Word8, Word64)
import qualified Data.ByteString.Lex.Integral as BSLex

----------------------------------------------------------------
----------------------------------------------------------------

-- Helper functions to ensure consistent strictness.
-- TODO: should we really be this strict?
{-# INLINE justPair #-}
justPair :: a -> b -> Maybe (a,b)
justPair x y
    | x `seq` y `seq` False = undefined
    | otherwise = Just (x,y)

{-# INLINE pair #-}
pair :: a -> b -> (a,b)
pair x y
    | x `seq` y `seq` False = undefined
    | otherwise = (x,y)


-- Predicates we use often
{-# INLINE isNotPeriod #-}
isNotPeriod :: Word8 -> Bool
isNotPeriod w = 0x2E /= w

{-# INLINE isNotE #-}
isNotE :: Word8 -> Bool
isNotE w = 0x65 /= w && 0x45 /= w

{-# INLINE isDecimal #-}
isDecimal :: Word8 -> Bool
isDecimal w = 0x39 >= w && w >= 0x30

----------------------------------------------------------------
-- | Version 1 of trying to speed things up. Is 3~3.5x faster than
-- the old Alex version, but still ~1.5x and ~3x slower than BSRead
-- at Float\/Double (on the short and long inputs, respectively; the
-- nonlinearity is no doubt due to BSRead's hackery about dropping
-- low-order bits). However, is ~1.3x and ~2x faster(!) than BSRead
-- at Rational
--
-- NOTE: We use 'fromInteger' everywhere instead of 'fromIntegral'
-- in order to fix the types of the calls to 'BSLex.readDecimal',
-- etc. This is always correct, but for some result types there are
-- other intermediate types which may be faster.
readDecimal1 :: (Fractional a) => ByteString -> Maybe (a, ByteString)
{-# SPECIALIZE readDecimal1 ::
    ByteString -> Maybe (Float,    ByteString),
    ByteString -> Maybe (Double,   ByteString),
    ByteString -> Maybe (Rational, ByteString) #-}
readDecimal1 xs0 =
    case BSLex.readDecimal xs0 of
    Nothing -> Nothing
    Just (whole, xs1)
        | BS.null xs1 || 0x2E /= BSU.unsafeHead xs1 ->
            justPair (fromInteger whole) xs1
        | otherwise ->
            case BSLex.readDecimal (BSU.unsafeTail xs1) of
            Nothing          -> justPair (fromInteger whole) xs1
            Just (part, xs2) ->
                let base = 10 ^ (BS.length xs1 - 1 - BS.length xs2)
                    frac = fromInteger whole + (fromInteger part / base)
                in justPair frac xs2

readExponential1 :: (Fractional a) => ByteString -> Maybe (a, ByteString)
{-# SPECIALIZE readExponential1 ::
    ByteString -> Maybe (Float,    ByteString),
    ByteString -> Maybe (Double,   ByteString),
    ByteString -> Maybe (Rational, ByteString) #-}
readExponential1 xs0 =
    -- TODO: benchmark the benefit of inlining 'readDecimal1' here
    case readDecimal1 xs0 of
    Nothing -> Nothing
    Just (f, xs1)
        | BS.null xs1 || (0x65 /= BSU.unsafeHead xs1 && 0x45 /= BSU.unsafeHead xs1) ->
            justPair f xs1
        | otherwise ->
            -- TODO: benchmark the benefit of inlining 'readSigned' here
            -- BUG: defaults to Integer... using 'fromInteger' on @e@ doesn't help...
            case BSLex.readSigned BSLex.readDecimal (BSU.unsafeTail xs1) of
            Nothing       -> justPair f xs1
            Just (e, xs2) -> justPair (f * (10 ^^ e)) xs2

----------------------------------------------------------------
-- | monomorphizing the exponent to 'Int'; for comparison against 'readExponential3'. Yep, that seems to do it; no need for inlining 'readDecimal1' and manually fusing away redundant cases
readExponential11 :: (Fractional a) => ByteString -> Maybe (a, ByteString)
{-# SPECIALIZE readExponential11 ::
    ByteString -> Maybe (Float,    ByteString),
    ByteString -> Maybe (Double,   ByteString),
    ByteString -> Maybe (Rational, ByteString) #-}
readExponential11 xs0 =
    case readDecimal1 xs0 of
    Nothing -> Nothing
    Just (f, xs1)
        | BS.null xs1 || (0x65 /= BSU.unsafeHead xs1 && 0x45 /= BSU.unsafeHead xs1) ->
            justPair f xs1
        | otherwise ->
            case BSLex.readSigned BSLex.readDecimal (BSU.unsafeTail xs1) of
            Nothing       -> justPair f xs1
            Just (e, xs2) -> justPair (f * (10 ^^ (e::Int))) xs2

----------------------------------------------------------------

-- | Inlining 'readDecimal1', and some minor strictness differences.
-- Is essentially the same; a little slower on Float\/Double, a
-- little faster on Rational.
readExponential2 :: (Fractional a) => ByteString -> Maybe (a, ByteString)
{-# SPECIALIZE readExponential2 ::
    ByteString -> Maybe (Float,    ByteString),
    ByteString -> Maybe (Double,   ByteString),
    ByteString -> Maybe (Rational, ByteString) #-}
readExponential2 = start
    where
    start xs0 =
        case BSLex.readDecimal xs0 of
        Nothing -> Nothing
        Just (whole, xs1)
            | BS.null xs1 || 0x2E /= BSU.unsafeHead xs1 ->
                Just $ readExponentPart (fromInteger whole) xs1
            | otherwise ->
                case BSLex.readDecimal (BSU.unsafeTail xs1) of
                Nothing          -> Just $ readExponentPart (fromInteger whole) xs1
                Just (part, xs2) ->
                    let base = 10 ^ (BS.length xs1 - 1 - BS.length xs2)
                        frac = fromInteger whole + (fromInteger part / base)
                    in Just $ readExponentPart frac xs2
    
    readExponentPart frac xs0
        | BS.null xs0 || (0x65 /= BSU.unsafeHead xs0 && 0x45 /= BSU.unsafeHead xs0) =
            pair frac xs0
        | otherwise =
            -- BUG: defaults to Integer... using 'fromInteger' on @e@ doesn't help...
            case BSLex.readSigned BSLex.readDecimal (BSU.unsafeTail xs0) of
            Nothing       -> pair frac xs0
            Just (e, xs1) -> pair (frac * (10 ^^ e)) xs1

----------------------------------------------------------------
-- | No longer collapsing identical branches. Doing only that is
-- essentially the same performance, but seems slightly slower on
-- average. N.B., two of the cases in @readDecimalPart@ can
-- short-circuit and avoid even trying to call @readExponentPart@.
-- However, we also monomorphize the exponent parsing to 'Int',
-- which gives a small but significant improvement across the board
-- (and it isn't even so small for Float\/Double). This gets us well
-- into the ballpark of @bytestring-read@ for the short input on
-- Float\/Double.
--
-- Up to ~4.3x and ~3.4x faster than the Alex original.
-- Still at ~1.3x and 1.9x faster than bytestring-read at Rational.
-- Still at ~0.90x to ~1.0x "faster" than bytestring-read at Float\/Double on the short input.
-- Still at ~0.3x "faster" than bytestring-read at Float\/Double on the long input.
readExponential3 :: (Fractional a) => ByteString -> Maybe (a, ByteString)
{-# SPECIALIZE readExponential3 ::
    ByteString -> Maybe (Float,    ByteString),
    ByteString -> Maybe (Double,   ByteString),
    ByteString -> Maybe (Rational, ByteString) #-}
readExponential3 = start
    where
    -- For 'Float' it's sufficient to use 'Word24' as the intermediate
    -- type, and for 'Double' it's sufficient to use 'Word53'.
    -- However, taking advantage of that requires fundeps or type
    -- families. The maximum value for that hack is @fromIntegral
    -- (floatRadix p) ^ floatDigits p@ for any 'RealFloat' proxy @p@
    start xs =
        case BSLex.readDecimal xs of
        Nothing           -> Nothing
        Just (whole, xs') -> Just $! readDecimalPart (fromInteger whole) xs'

    readDecimalPart whole xs
        | whole `seq` False               = undefined
        | BS.null xs                      = pair whole BS.empty
        | isNotPeriod (BSU.unsafeHead xs) = readExponentPart whole xs
        | otherwise                       =
            case BSLex.readDecimal (BSU.unsafeTail xs) of
            Nothing          -> pair whole xs
            Just (part, xs') ->
                let base = 10 ^ (BS.length xs - 1 - BS.length xs')
                    frac = whole + (fromInteger part / base)
                    -- TODO: it'd be more robust (but slower?) to use: @(whole*base + fromInteger part) / base@
                in readExponentPart frac xs'

    readExponentPart frac xs
        | frac `seq` False           = undefined
        | BS.null xs                 = pair frac BS.empty
        | isNotE (BSU.unsafeHead xs) = pair frac xs
        | otherwise                  =
            -- According to 'RealFrac' exponents should be 'Int'..., so using that instead of 'Integer' to avoid defaulting here.
            case BSLex.readSigned BSLex.readDecimal (BSU.unsafeTail xs) of
            Nothing       -> pair frac xs
            Just (e, xs') -> pair (frac * (10 ^^ (e::Int))) xs'

----------------------------------------------------------------
-- | Trying the 'RealFloat' trick from @bytestring-read@. Wow that's fast!
--
-- Is ~1.85x to ~2.0x faster at Float\/Double on the short input.
-- Is ~1.58x faster at Float\/Double on the long input.
readExponential4 :: forall a. (RealFloat a) => ByteString -> Maybe (a, ByteString)
{-# SPECIALIZE readExponential4 ::
    ByteString -> Maybe (Float,  ByteString),
    ByteString -> Maybe (Double, ByteString) #-}
readExponential4 = start
    where
    -- TODO: double check that this proxy code gets optimized away in Core
    -- TODO: how can we get our hands on this type without ScopedTypeVariables?
    proxy :: a
    proxy = undefined
    
    -- Double's max fractional value is 9007199254740992, length=16
    -- Float's max fractional value is 16777216, length=8
    magicLength = length . show $ (floatRadix proxy ^ floatDigits proxy)
    
    -- For 'Float' it's sufficient to use 'Word24' as the intermediate
    -- type, and for 'Double' it's sufficient to use 'Word53'.
    -- HACK: the first input type should be specified by a fundep or typefamily!
    {-# INLINE fromFraction #-}
    fromFraction :: Word64 -> Int -> a
    fromFraction frac scale = fromIntegral frac * (10 ^^ scale)
    
    -- BUG: need to deal with leading zeros re 'magicLength'
    start :: ByteString -> Maybe (a, ByteString)
    start xs
        | BS.length xs <= magicLength = readExponential3 xs
        | otherwise                   =
            case BSLex.readDecimal (BS.take magicLength xs) of
            Nothing          -> Nothing
            Just (frac, ys)
                | BS.null ys ->
                    let scale = BS.length
                              . BS.takeWhile isDecimal
                              $ BS.drop magicLength xs
                    in Just $! dropDecimalPart frac scale
                        (BS.drop (magicLength+scale) xs)
                | otherwise  ->
                    let len = BS.length ys in
                    Just $! readDecimalPart frac len
                        (BS.drop (magicLength-len) xs)

    dropDecimalPart :: Word64 -> Int -> ByteString -> (a, ByteString)
    dropDecimalPart frac scale xs
        | frac `seq` scale `seq` False = undefined
        | BS.null xs = pair (fromFraction frac scale) BS.empty
        | otherwise  = readExponentPart frac scale $!
            if isNotPeriod (BSU.unsafeHead xs)
            then xs
            else BS.dropWhile isDecimal (BSU.unsafeTail xs)
    
    readDecimalPart :: Word64 -> Int -> ByteString -> (a, ByteString)
    readDecimalPart frac len xs
        | frac `seq` len `seq` False      = undefined
        | BS.null xs                      = error "readExponential4.readDecimalPart: impossible"
        | isNotPeriod (BSU.unsafeHead xs) = readExponentPart frac 0 xs
        | otherwise                       =
            let ys = BS.take len (BSU.unsafeTail xs) in
            case BSLex.readDecimal ys of
            Nothing           -> pair (fromIntegral frac) xs
            Just (part, ys')
                | BS.null ys' ->
                    let scale = BS.length ys in
                    readExponentPart
                        (frac * (10 ^ scale) + part)
                        (negate scale)
                        (BS.dropWhile isDecimal (BS.drop (1+scale) xs))
                | otherwise   ->
                    let scale = BS.length ys - BS.length ys' in
                    readExponentPart
                        (frac * (10 ^ scale) + part)
                        (negate scale)
                        (BS.drop (1+scale) xs)

    readExponentPart :: Word64 -> Int -> ByteString -> (a, ByteString)
    readExponentPart frac scale xs
        | frac `seq` scale `seq` False = undefined
        | BS.null xs                 = pair (fromFraction frac scale) BS.empty
        | isNotE (BSU.unsafeHead xs) = pair (fromFraction frac scale) xs
        | otherwise                  =
            -- According to 'RealFrac' exponents should be 'Int'..., so using that instead of 'Integer' to avoid defaulting here.
            case BSLex.readSigned BSLex.readDecimal (BSU.unsafeTail xs) of
            Nothing       -> pair (fromFraction frac scale) xs
            Just (e, xs') -> pair (fromFraction frac (scale + e)) xs'


----------------------------------------------------------------
-- | Backpatching 'readExponential3' with the fusion of exponentiation
-- trick in 'readExponential4'. Way slower on Float\/Double; but way
-- faster on Rational.
readExponential31 :: (Fractional a) => ByteString -> Maybe (a, ByteString)
{-# SPECIALIZE readExponential31 ::
    ByteString -> Maybe (Float,    ByteString),
    ByteString -> Maybe (Double,   ByteString),
    ByteString -> Maybe (Rational, ByteString) #-}
readExponential31 = start
    where
    {-# INLINE fromFraction #-}
    fromFraction :: (Fractional a) => Integer -> Int -> a
    fromFraction frac scale = fromInteger frac * (10 ^^ scale)
    
    start xs =
        case BSLex.readDecimal xs of
        Nothing           -> Nothing
        Just (whole, xs') -> Just $! readDecimalPart whole xs'

    readDecimalPart whole xs
        | whole `seq` False               = undefined
        | BS.null xs                      = pair (fromInteger whole) BS.empty
        | isNotPeriod (BSU.unsafeHead xs) = readExponentPart whole 0 xs
        | otherwise                       =
            case BSLex.readDecimal (BSU.unsafeTail xs) of
            Nothing          -> pair (fromInteger whole) xs
            Just (part, xs') ->
                let scale = BS.length xs - 1 - BS.length xs'
                    frac = whole * (10 ^ scale) + part
                in readExponentPart frac (negate scale) xs'

    readExponentPart frac scale xs
        | frac `seq` False           = undefined
        | BS.null xs                 = pair (fromFraction frac scale) BS.empty
        | isNotE (BSU.unsafeHead xs) = pair (fromFraction frac scale) xs
        | otherwise                  =
            -- According to 'RealFrac' exponents should be 'Int'..., so using that instead of 'Integer' to avoid defaulting here.
            case BSLex.readSigned BSLex.readDecimal (BSU.unsafeTail xs) of
            Nothing       -> pair (fromFraction frac scale) xs
            Just (e, xs') -> pair (fromFraction frac (scale + e)) xs'


----------------------------------------------------------------
-- | Moving around the 'fromInteger' conversion in 'readExponential31'.
-- Better for Float\/Double (though still slower than 'readExponential3');
-- worse for Rational (the short test is slower than 'readExponential3',
-- but the long test is still faster than 'readExponential3').
readExponential32 :: forall a. (Fractional a) => ByteString -> Maybe (a, ByteString)
{-# SPECIALIZE readExponential32 ::
    ByteString -> Maybe (Float,    ByteString),
    ByteString -> Maybe (Double,   ByteString),
    ByteString -> Maybe (Rational, ByteString) #-}
readExponential32 = start
    where
    {-# INLINE fromFraction #-}
    fromFraction :: a -> Int -> a
    fromFraction frac scale = frac * (10 ^^ scale)
    
    start xs =
        case BSLex.readDecimal xs of
        Nothing           -> Nothing
        Just (whole, xs') -> Just $! readDecimalPart (fromInteger whole) xs'

    readDecimalPart :: a -> ByteString -> (a,ByteString)
    readDecimalPart whole xs
        | whole `seq` False               = undefined
        | BS.null xs                      = pair whole BS.empty
        | isNotPeriod (BSU.unsafeHead xs) = readExponentPart whole 0 xs
        | otherwise                       =
            case BSLex.readDecimal (BSU.unsafeTail xs) of
            Nothing          -> pair whole xs
            Just (part, xs') ->
                let scale = BS.length xs - 1 - BS.length xs'
                    -- TODO: @Data.Ratio.numerator whole * (10...@
                    frac = whole * (10 ^ scale) + fromInteger part
                in readExponentPart frac (negate scale) xs'

    readExponentPart :: a -> Int -> ByteString -> (a,ByteString)
    readExponentPart frac scale xs
        | frac `seq` False           = undefined
        | BS.null xs                 = pair (fromFraction frac scale) BS.empty
        | isNotE (BSU.unsafeHead xs) = pair (fromFraction frac scale) xs
        | otherwise                  =
            -- According to 'RealFrac' exponents should be 'Int'..., so using that instead of 'Integer' to avoid defaulting here.
            case BSLex.readSigned BSLex.readDecimal (BSU.unsafeTail xs) of
            Nothing       -> pair (fromFraction frac scale) xs
            Just (e, xs') -> pair (fromFraction frac (scale + e)) xs'

----------------------------------------------------------------
----------------------------------------------------------------
{-
-- TODO: something like this? (a~la bytestring-read)
-- TODO: use MPTCs and FunDeps instead of type families

class Fractional a => DecimalFractional a where
    -- So we can give a type to 'toDF'
    data Significand a :: *
    
    -- So we can define 'fromDF' generically
    fromSignificand :: Significand a -> a
    
    -- Should generally have the form:
    -- > DF {-# UNPACK #-}!(Significand a) {-# UNPACK #-}!Int
    -- But we must put it in the class in order to be able to unpack
    data DecimalFraction a :: *
    
    -- | Generic constructor for 'DecimalFraction'.
    toDF :: Significand a -> Int -> DecimalFraction a
    
    -- | Generic destructors for 'DecimalFraction'; so we can define 'fromDF' generically.
    significandDF :: DecimalFraction a -> Significand a
    exponentDF :: DecimalFraction a -> Int
    
    -- May need to inline the default definition in order to avoid repeated pattern matching...
    fromDF :: DecimalFractional a => DecimalFraction a -> a
    fromDF df = fromSignificand (significandDF df) * (10 ^^ exponentDF df)
    {-# INLINE fromDF #-}
    
    -- May need to inline the default definition in order to avoid repeated pattern matching...
    scaleDF :: DecimalFraction a -> Int -> DecimalFraction a
    scaleDF df scale = toDF (significandDF df) (exponentDF df + scale)
    {-# INLINE scaleDF #-}
    
    -- Doesn't really belong here...
    digitsPrecision :: proxy a -> Maybe Int

digitsPrecisionRealFloat :: forall a. RealFloat a => proxy a -> Maybe Int
{-# INLINE digitsPrecisionRealFloat #-}
digitsPrecisionRealFloat = \_ ->
        let proxy = undefined::a
        in  Just . length . show $ (floatRadix proxy ^ floatDigits proxy)

instance DecimalFractional Float where
    type Significand Float = Word32
    fromSignificand = fromIntegral
    
    data DecimalFraction Float =
        DF_F {-# UNPACK #-}!Word32 {-# UNPACK #-}!Int
    toDF = DF_F
    significandDF (DF_F frac _)     = frac
    exponentDF    (DF_F _    scale) = scale
    fromDF        (DF_F frac scale) = fromIntegral frac * (10 ^^ scale)
    scaleDF       (DF_F frac scale) scale' = DF_F frac (scale + scale')
    
    digitsPrecision = digitsPrecisionRealFloat
    
    {-# INLINE fromSignificand #-}
    {-# INLINE toDF #-}
    {-# INLINE significandDF #-}
    {-# INLINE exponentDF #-}
    {-# INLINE fromDF #-}
    {-# INLINE scaleDF #-}
    {-# INLINE digitsPrecision #-}

instance DecimalFractional Double where
    type Significand Double = Word64
    fromSignificand = fromIntegral
    
    data DecimalFraction Double =
        DF_D {-# UNPACK #-}!Word64 {-# UNPACK #-}!Int
    toDF = DF_D
    significandDF (DF_D frac _)     = frac
    exponentDF    (DF_D _    scale) = scale
    fromDF        (DF_D frac scale) = fromIntegral frac * (10 ^^ scale)
    scaleDF       (DF_D frac scale) scale' = DF_D frac (scale + scale')
    
    digitsPrecision = digitsPrecisionRealFloat
    
    {-# INLINE fromSignificand #-}
    {-# INLINE toDF #-}
    {-# INLINE significandDF #-}
    {-# INLINE exponentDF #-}
    {-# INLINE fromDF #-}
    {-# INLINE scaleDF #-}
    {-# INLINE digitsPrecision #-}

instance Integral a => DecimalFractional (Ratio a) where
    type Significand (Ratio a) = a
    fromSignificand = fromIntegral
    
    -- Might be able to unpack certain @a@, but not all of them, and not 'Integer' specifically.
    data DecimalFraction (Ratio a) = DF_R !a {-# UNPACK #-}!Int
    toDF = DF_R
    significandDF (DF_R frac _)     = frac
    exponentDF    (DF_R _    scale) = scale
    fromDF        (DF_R frac scale) = fromIntegral frac * (10 ^^ scale)
    scaleDF       (DF_R frac scale) scale' = DF_R frac (scale + scale')
    
    digitsPrecision = \_ -> Nothing -- could be wrong for non-Integer @a@
    
    {-# INLINE fromSignificand #-}
    {-# INLINE toDF #-}
    {-# INLINE significandDF #-}
    {-# INLINE exponentDF #-}
    {-# INLINE fromDF #-}
    {-# INLINE scaleDF #-}
    {-# INLINE digitsPrecision #-}
-}

-- BUG: Can't unpack integers...
-- BUG: ought to be able to specify any Integral (Num?) type based on @a@
--     Float ==> Word24; Double ==> Word53; Ratio a ==> a
data DecimalFraction a = DF {-UNPACK-}!Integer {-# UNPACK #-}!Int

{-# INLINE fromDF #-}
fromDF :: Fractional a => DecimalFraction a -> a
fromDF (DF frac scale) = fromInteger frac * (10 ^^ scale)

{-# INLINE scaleDF #-}
scaleDF :: Fractional a => DecimalFraction a -> Int -> DecimalFraction a
scaleDF (DF frac scale) scale' = DF frac (scale + scale')

-- | A variant of 'readDecimal1' factored out from 'readExponential4', for use in composition with 'readExponential41'. Also, removing the bail out case when @BS.length xs <= p@.
readDecimal41 :: (Fractional a) => Int -> ByteString -> Maybe (DecimalFraction a, ByteString)
{-# SPECIALIZE readDecimal41 ::
    Int -> ByteString -> Maybe (DecimalFraction Float,    ByteString),
    Int -> ByteString -> Maybe (DecimalFraction Double,   ByteString),
    Int -> ByteString -> Maybe (DecimalFraction Rational, ByteString) #-}
readDecimal41 = start
    where
    -- BUG: need to deal with leading zeros re @p@
    -- TODO: verify this is inferred strict in both @p@ and @xs@
    start p xs = 
        let ys = BS.take p xs in
        -- monomorphic at type 'Integer' because that's what 'DF' requires
        case BSLex.readDecimal ys of
        Nothing          -> Nothing
        Just (whole, ys')
            | BS.null ys' ->
                -- TODO: inline dropDecimalPart?
                let scale = BS.length
                          . BS.takeWhile isDecimal
                          $ BS.drop p xs
                in Just $! dropDecimalPart whole scale
                    (BS.drop (p+scale) xs)
            | otherwise  ->
                -- N.B., if @p > BS.length xs@ then can't use the old implementation from readExponential4!
                let len = BS.length ys - BS.length ys' in
                Just $! readDecimalPart (p-len) whole
                    (BS.drop len xs)

    dropDecimalPart whole scale xs
        | whole `seq` scale `seq` False = undefined
        | BS.null xs = pair (DF whole scale) BS.empty
        | otherwise  = pair (DF whole scale) $!
            if isNotPeriod (BSU.unsafeHead xs)
            then xs
            else BS.dropWhile isDecimal (BSU.unsafeTail xs)
    
    readDecimalPart p whole xs
        | p `seq` whole `seq` False       = undefined
        | BS.null xs                      = error "the impossible happened"
        | isNotPeriod (BSU.unsafeHead xs) = pair (DF whole 0) xs
        | otherwise                       =
            let ys = BS.take p (BSU.unsafeTail xs) in
            -- monomorphic at type 'Integer' because that's what 'DF' requires
            case BSLex.readDecimal ys of
            Nothing           -> pair (DF whole 0) xs
            Just (part, ys')
                | BS.null ys' ->
                    let scale = BS.length ys in
                    pair (DF (whole * (10 ^ scale) + part) (negate scale))
                        (BS.dropWhile isDecimal (BS.drop (1+scale) xs))
                | otherwise   ->
                    let scale = BS.length ys - BS.length ys' in
                    pair (DF (whole * (10 ^ scale) + part) (negate scale))
                        (BS.drop (1+scale) xs)


-- | A variant of 'readExponential4' with (a) the 'readDecimal41' refactored out, and (b) the precision limit passed as an explicit parameter.
--
-- Results: it's marginally slower than 'readExponential4' at Float\/Double with their magic numbers; which could be noise, or could be because of using 'Integer' for the significands. And it's on par with 'readExponential31' for Rational with infinity (i.e., the length of the string plus one).
readExponential41 :: (Fractional a) => Int -> ByteString -> Maybe (a, ByteString)
{-# SPECIALIZE readExponential41 ::
    Int -> ByteString -> Maybe (Float,    ByteString),
    Int -> ByteString -> Maybe (Double,   ByteString),
    Int -> ByteString -> Maybe (Rational, ByteString) #-}
readExponential41 = start
    where
    start p xs =
        case readDecimal41 p xs of
        Nothing       -> Nothing
        Just (df,xs') -> Just $! readExponentPart df xs'
        -- Short circuits lost by the refactoring: We're done if...
        -- * we dropped the decimal part but it was null
        -- * we read the decimal part but parsing it failed
    
    readExponentPart df xs
        | BS.null xs                 = pair (fromDF df) BS.empty
        | isNotE (BSU.unsafeHead xs) = pair (fromDF df) xs
        | otherwise                  =
            -- HACK: monomorphizing at 'Int'
            -- TODO: how to handle too-large exponents?
            case BSLex.readSigned BSLex.readDecimal (BSU.unsafeTail xs) of
            Nothing          -> pair (fromDF df) xs
            Just (expn, xs') -> pair (fromDF $ scaleDF df expn) xs'


----------------------------------------------------------------
----------------------------------------------------------- fin.