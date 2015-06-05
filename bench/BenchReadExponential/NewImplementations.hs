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
-- A bunch of new implementations for parsing floating point numbers.
----------------------------------------------------------------
module BenchReadExponential.NewImplementations
    ( readExponential1
    , readExponential11 -- Implementation of choice for general fractional
    , readExponential2
    , readExponential3
    , readExponential31
    , readExponential32
    , readExponential4
    , readExponential41 -- Implementation of choice for limited precision
    -- TODO: a version of readExponential41 which always does infinite precision; can give NaNs instead of Inftys, but that may be acceptable? The big reason to try this is in case dropping the extra variable lets us avoid spilling registers. Could also actually try looking at the assembly...
    , readExponential42
    --
    , decimalPrecision
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
{-# INLINE isPeriod #-}
isPeriod :: Word8 -> Bool
isPeriod w = w == 0x2E

{-# INLINE isNotPeriod #-}
isNotPeriod :: Word8 -> Bool
isNotPeriod w = w /= 0x2E

{-# INLINE isNotE #-}
isNotE :: Word8 -> Bool
isNotE w = w /= 0x65 && w /= 0x45

{-# INLINE isDecimal #-}
isDecimal :: Word8 -> Bool
isDecimal w = 0x39 >= w && w >= 0x30

{-# INLINE isDecimalZero #-}
isDecimalZero :: Word8 -> Bool
isDecimalZero w = w == 0x30

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
-- average. N.B., two of the cases in @readFractionPart@ can
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
        Just (whole, xs') -> Just $! readFractionPart (fromInteger whole) xs'

    readFractionPart whole xs
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
                    in Just $! dropFractionPart frac scale
                        (BS.drop (magicLength+scale) xs)
                | otherwise  ->
                    let len = BS.length ys in
                    Just $! readFractionPart frac len
                        (BS.drop (magicLength-len) xs)

    dropFractionPart :: Word64 -> Int -> ByteString -> (a, ByteString)
    dropFractionPart frac scale xs
        | frac `seq` scale `seq` False = undefined
        | BS.null xs = pair (fromFraction frac scale) BS.empty
        | otherwise  = readExponentPart frac scale $!
            if isNotPeriod (BSU.unsafeHead xs)
            then xs
            else BS.dropWhile isDecimal (BSU.unsafeTail xs) -- N.B., buggy!
    
    readFractionPart :: Word64 -> Int -> ByteString -> (a, ByteString)
    readFractionPart frac len xs
        | frac `seq` len `seq` False      = undefined
        | BS.null xs                      = error "readExponential4.readFractionPart: impossible"
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
        Just (whole, xs') -> Just $! readFractionPart whole xs'

    readFractionPart whole xs
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
        Just (whole, xs') -> Just $! readFractionPart (fromInteger whole) xs'

    readFractionPart :: a -> ByteString -> (a,ByteString)
    readFractionPart whole xs
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

{-# INLINE fractionDF #-}
fractionDF :: Integer -> Int -> Integer -> DecimalFraction a
fractionDF whole scale part =
    DF (whole * (10 ^ scale) + part) (negate scale)
    -- TODO: use an unsafe variant of (^) which has an assertion instead of a runtime check?

{-# INLINE fromDF #-}
fromDF :: Fractional a => DecimalFraction a -> a
fromDF (DF frac scale)
    -- Avoid possibility of returning NaN
    -- TODO: really, ought to check @fromInteger frac == 0@...
    | frac  == 0        = 0
    -- Avoid throwing an error due to @negate minBound == minBound@
    | scale == minBound = fromInteger frac * (10 ^^ toInteger scale)
    -- Now we're safe for the default implementation
    | otherwise         = fromInteger frac * (10 ^^ scale)
    -- TODO: manually implement (^^) so that we get @_ / (10^ _)@ instead of @_ * recip (10^ _)@ for negative exponents?

{-# INLINE scaleDF #-}
scaleDF :: DecimalFraction a -> Int -> DecimalFraction a
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
        Nothing           -> Nothing
        Just (whole, ys')
            | BS.null ys' ->
                -- TODO: inline dropFractionPart?
                let scale = BS.length
                          . BS.takeWhile isDecimal
                          $ BS.drop p xs
                in Just $! dropFractionPart whole scale
                    (BS.drop (p+scale) xs)
            | otherwise  ->
                -- N.B., if @p > BS.length xs@ then can't use the old implementation from readExponential4!
                let len = BS.length ys - BS.length ys' in
                Just $! readFractionPart (p-len) whole
                    (BS.drop len xs)

    dropFractionPart whole scale xs
        | whole `seq` scale `seq` False = undefined
        | BS.null xs = pair (DF whole scale) BS.empty
        | otherwise  = pair (DF whole scale) $!
            if isNotPeriod (BSU.unsafeHead xs)
            then xs
            else BS.dropWhile isDecimal (BSU.unsafeTail xs) -- N.B., is buggy
    
    readFractionPart p whole xs
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
                    pair (fractionDF whole scale part)
                        (BS.dropWhile isDecimal (BS.drop (1+scale) xs))
                | otherwise   ->
                    let scale = BS.length ys - BS.length ys' in
                    pair (fractionDF whole scale part)
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
            Nothing           -> pair (fromDF df) xs
            Just (scale, xs') -> pair (fromDF $ scaleDF df scale) xs'


-- TODO: is there any way to avoid ScopedTypeVariables without losing the fact that this is a constant function?
-- TODO: try looking at core again to see if @n@ gets completely optimized away or not. If not, is there a way to help that happen without using TH?
decimalPrecision :: forall proxy a. RealFloat a => proxy a -> Int
{-# INLINE decimalPrecision #-}
decimalPrecision =
    let n = let proxy = (undefined :: a)
            -- TODO: use numDecimalDigits!!
            in length . show $ (floatRadix proxy ^ floatDigits proxy)
    in n `seq` \_ -> n

-- 16777216 is the maximum significand for Float
-- fromDF(DF 16777216 31)::Float hits Infinity (exponent is 301 for Double)
-- also fromDF(DF 1 39)::Float hits Infinity
-- fromDF(DF 16777216 maxBound) is still merely Infinity.
-- Underflows to zero at fromDF(DF 16777216 (-309)); fromDF(DF 16777216 (-308)) is fine; 308 == 2^8 + 2^5 + 2^4 + 2^2. Still no NaN...
-- BUG: where the heck does the NaN come from??
-- Aha! fromDF(DF 0 39)::Float == NaN !! Easy enough to fix... but why would parsing the \"long\" example in BenchReadExponential.hs result in a DF with 0 significand?


----------------------------------------------------------------

-- TODO: use 'BS.breakByte' where possible; or design our own similar...
-- BUG: can't use 'BS.break'...
-- | Break on the predicate (i.e., 'takeWhile' it is false!), returning the length of what matched and then the remainder that didn't
lengthDropWhile :: (Word8 -> Bool) -> ByteString -> (Int, ByteString)
{-# INLINE lengthDropWhile #-}
lengthDropWhile p xs =
    let ys = BS.dropWhile p xs
    in (BS.length xs - BS.length ys, ys)
    {-
    -- TODO: benchmark
    let len = BS.length (BS.takeWhile p xs)
    in (len, BS.drop len xs)
    -}

-- Some test cases to ensure we hit most\/all branches:
-- > let c = "01a." in concat [ Control.Monad.replicateM i c | i <- [0..4]]
-- TODO: add HUnit tests for these sorts of things; especially "0.", ".", "0.a", "01.", and "0.01" Also, use HPC on those tests
--
-- A good testcase for the leading zeroes:
-- > readExponential41 10 $ BS8.pack "0000000000000000000000000.123456789"
-- > readExponential42 10 $ BS8.pack "0000000000000000000000000.123456789"
-- N.B., for this test readExponential11 and readExponential42 are correct, but readExponential41 is wrong

-- | A variant of 'readDecimal41' trying to fix the bug about leading zeros. Also the other bug about accepting @\"[0-9]+\\.[^0-9].*\"@.
readDecimal42 :: (Fractional a) => Int -> ByteString -> Maybe (DecimalFraction a, ByteString)
{-# SPECIALIZE readDecimal42 ::
    Int -> ByteString -> Maybe (DecimalFraction Float,    ByteString),
    Int -> ByteString -> Maybe (DecimalFraction Double,   ByteString),
    Int -> ByteString -> Maybe (DecimalFraction Rational, ByteString) #-}
readDecimal42 = start
    where
    -- All calls to 'BSLex.readDecimal' are monomorphized at 'Integer', as specified by what 'DF' needs.
    
    -- TODO: verify this is ~inferred~ strict in both @p@ and @xs@ without the guard trick or BangPatterns
    start p xs
        | p `seq` xs `seq` False = undefined
        | otherwise =
            case lengthDropWhile isDecimalZero xs of
            (0, _)  -> readWholePart p xs
            (_, ys) ->
                case BS.uncons ys of
                Nothing              -> justPair (DF 0 0) BS.empty
                Just (y0,ys0)
                    | isDecimal   y0 -> readWholePart p ys
                    | isNotPeriod y0 -> justPair (DF 0 0) ys
                    | otherwise      ->
                        case lengthDropWhile isDecimalZero ys0 of
                        (0,     _)   -> readFractionPart p 0 ys
                        (scale, zs)  -> afterDroppingZeroes p scale zs
    
    afterDroppingZeroes p scale xs =
        let ys = BS.take p xs in
        case BSLex.readDecimal ys of
        Nothing          -> justPair (DF 0 0) xs
        Just (part, ys') ->
            let scale' = scale + BS.length xs - BS.length ys'
            in  justPair (DF part (negate scale'))
                    (BS.dropWhile isDecimal ys')
    
    readWholePart p xs =
        let ys = BS.take p xs in
        case BSLex.readDecimal ys of
        Nothing           -> Nothing
        Just (whole, ys')
            | BS.null ys' ->
                case lengthDropWhile isDecimal (BS.drop p xs) of
                (scale, zs) ->
                    justPair (DF whole scale) (dropFractionPart zs)
            | otherwise  ->
                let len = BS.length ys - BS.length ys'
                    -- N.B., @xs' == ys' `BS.append` BS.drop p xs@
                    xs' = BS.drop len xs
                in
                -- N.B., @BS.null xs'@ is impossible. Were it to happen then returning @pair (DF whole 0) BS.empty@ is consistent with the branch where we drop the fraction part (the original input is less than the original @p@ long); however, reaching this branch via that input would be a control-flow error.
                if isPeriod (BSU.unsafeHead xs')
                then readFractionPart (p-len) whole xs'
                else justPair (DF whole 0) xs'

    -- TODO: verify that 'BS.uncons' is inlined and that the intermediate @Maybe(,)@ it returns is fused away
    dropFractionPart xs =
        case BS.uncons xs of
        Nothing                    -> BS.empty -- == xs
        Just (x0,xs0)
            | isNotPeriod x0       -> xs
            | otherwise            ->
                case BS.uncons xs0 of
                Nothing            -> BS.singleton 0x2E -- == xs
                Just (x1,xs1)
                    | isDecimal x1 -> BS.dropWhile isDecimal xs1
                    | otherwise    -> xs
    
    -- N.B., @BS.null xs@ is impossible as it begins with a period; see the call sites.
    -- If @not (BS.null ys')@ then the @BS.dropWhile isDecimal@ is a noop; but there's no reason to branch on testing for that.
    -- The @+1@ in @BS.drop (1+scale)@ is for the 'BSU.unsafeTail' in @ys@.
    readFractionPart p whole xs =
        let ys = BS.take p (BSU.unsafeTail xs) in
        case BSLex.readDecimal ys of
        Nothing          -> justPair (DF whole 0) xs
        Just (part, ys') ->
            let scale = BS.length ys - BS.length ys'
            in  justPair (fractionDF whole scale part)
                    (BS.dropWhile isDecimal (BS.drop (1+scale) xs))
                


-- | A variant of 'readExponential41' using 'readDecimal42'.
readExponential42 :: (Fractional a) => Int -> ByteString -> Maybe (a, ByteString)
{-# SPECIALIZE readExponential42 ::
    Int -> ByteString -> Maybe (Float,    ByteString),
    Int -> ByteString -> Maybe (Double,   ByteString),
    Int -> ByteString -> Maybe (Rational, ByteString) #-}
readExponential42 = start
    where
    start p xs =
        case readDecimal42 p xs of
        Nothing       -> Nothing
        Just (df,xs') -> Just $! readExponentPart df xs'
    
    readExponentPart df xs
        | BS.null xs                 = pair (fromDF df) BS.empty
        | isNotE (BSU.unsafeHead xs) = pair (fromDF df) xs
        | otherwise                  =
            -- HACK: monomorphizing at 'Int'
            -- TODO: how to handle too-large exponents?
            case BSLex.readSigned BSLex.readDecimal (BSU.unsafeTail xs) of
            Nothing           -> pair (fromDF df) xs
            Just (scale, xs') -> pair (fromDF $ scaleDF df scale) xs'


----------------------------------------------------------------
----------------------------------------------------------- fin.