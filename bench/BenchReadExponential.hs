{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# LANGUAGE ScopedTypeVariables #-}
----------------------------------------------------------------
--                                                    2015.06.02
-- |
-- Module      :  BenchReadExponential
-- Copyright   :  Copyright (c) 2015 wren gayle romano,
--                              2015 Hirotomo Moriwaki
-- License     :  BSD2
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  benchmark
-- Portability :  ScopedTypeVariables
--
-- Benchmark the speed of parsing floating point numbers. This
-- benchmark originally came from @bytestring-read@ version 0.3.0.
----------------------------------------------------------------
module BenchReadExponential (main) where

import           Criterion.Main
import           Control.DeepSeq                (NFData)
import           Data.ByteString                (ByteString)
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Unsafe         as BSU
import qualified Data.ByteString.Char8          as BS8
import           Data.Word                      (Word8, Word64)
import qualified Test.QuickCheck                as QC
import qualified Data.ByteString.Read           as BSRead
import qualified BenchReadExponential.Double    as BSLexOld
import qualified Data.ByteString.Lex.Integral   as BSLex

----------------------------------------------------------------
----------------------------------------------------------------

-- | A helper function to ensure consistent strictness.
-- TODO: should we really be this strict?
justPair :: a -> b -> Maybe (a,b)
justPair x y
    | x `seq` y `seq` False = undefined
    | otherwise = Just (x,y)
{-# INLINE justPair #-}

-- | Version 1 of trying to speed things up. Is 3~3.5x faster than
-- the old Alex version, but still ~1.5x and ~3x slower than BSRead
-- at Float/Double (on the short and long inputs, respectively; the
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
-- | Inlining 'readDecimal1', and some minor strictness differences.
-- Is essentially the same; a little slower on Float/Double, a
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
            -- TODO: benchmark the benefit of inlining 'readSigned' here
            -- BUG: defaults to Integer... using 'fromInteger' on @e@ doesn't help...
            case BSLex.readSigned BSLex.readDecimal (BSU.unsafeTail xs0) of
            Nothing       -> pair frac xs0
            Just (e, xs1) -> pair (frac * (10 ^^ e)) xs1

    {-# INLINE pair #-}
    pair x y
        | x `seq` y `seq` False = undefined
        | otherwise = (x,y)

----------------------------------------------------------------
-- | No longer collapsing identical branches. Doing only that is
-- essentially the same performance, but seems slightly slower on
-- average. N.B., two of the cases in @readDecimalPart@ can short-circuit and avoid even trying to call @readExponentPart@. However, we also monomorphize the exponent parsing to 'Int', which gives a small but significant improvement across the board (and it isn't even so small for Float/Double). This gets us well into the ballpark of @bytestring-read@ for the short input on Float/Double.
--
-- Up to ~4.3x and ~3.4x faster than the Alex original.
-- Still at ~1.3x and 1.9x faster than bytestring-read at Rational.
-- Still at ~0.90x to ~1.0x "faster" than bytestring-read at Float/Double on the short input.
-- Still at ~0.3x "faster" than bytestring-read at Float/Double on the long input.
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
            -- TODO: benchmark the benefit of inlining 'readSigned' here
            -- According to 'RealFrac' exponents should be 'Int'..., so using that instead of 'Integer' to avoid defaulting here.
            case BSLex.readSigned BSLex.readDecimal (BSU.unsafeTail xs) of
            Nothing       -> pair frac xs
            Just (e, xs') -> pair (frac * (10 ^^ (e::Int))) xs'

    {-# INLINE isNotPeriod #-}
    isNotPeriod w = 0x2E /= w

    {-# INLINE isNotE #-}
    isNotE w = 0x65 /= w && 0x45 /= w

    {-# INLINE pair #-}
    pair x y
        | x `seq` y `seq` False = undefined
        | otherwise = (x,y)

----------------------------------------------------------------
-- | Trying the 'RealFloat' trick from @bytestring-read@. Wow that's fast!
--
-- Is ~1.85x to ~2.0x faster at Float/Double on the short input.
-- Is ~1.58x faster at Float/Double on the long input.
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
            -- TODO: benchmark the benefit of inlining 'readSigned' here
            -- According to 'RealFrac' exponents should be 'Int'..., so using that instead of 'Integer' to avoid defaulting here.
            case BSLex.readSigned BSLex.readDecimal (BSU.unsafeTail xs) of
            Nothing       -> pair (fromFraction frac scale) xs
            Just (e, xs') -> pair (fromFraction frac (scale + e)) xs'

    {-# INLINE isNotPeriod #-}
    isNotPeriod :: Word8 -> Bool
    isNotPeriod w = 0x2E /= w

    {-# INLINE isNotE #-}
    isNotE :: Word8 -> Bool
    isNotE w = 0x65 /= w && 0x45 /= w

    {-# INLINE isDecimal #-}
    isDecimal :: Word8 -> Bool
    isDecimal w = 0x39 >= w && w >= 0x30
    
    {-# INLINE pair #-}
    pair x y
        | x `seq` y `seq` False = undefined
        | otherwise = (x,y)


----------------------------------------------------------------
----------------------------------------------------------------

unwrap :: Maybe (a, ByteString) -> a
{-# INLINE unwrap #-}
unwrap Nothing   = error "couldn't parse input at all"
unwrap (Just (n,xs))
    | BS.null xs = n
    | otherwise  = error ("input not fully parsed: " ++ BS8.unpack xs)

----------------------------------------------------------------
-- The old version used by bytestring-lexing
readDouble               :: ByteString -> Double
readDouble               = unwrap . BSLexOld.readDouble

-- The suggested new versions
readExponential1_Float    :: ByteString -> Float
readExponential1_Float    = unwrap . BSLex.readSigned readExponential1
readExponential1_Double   :: ByteString -> Double
readExponential1_Double   = unwrap . BSLex.readSigned readExponential1
readExponential1_Rational :: ByteString -> Rational
readExponential1_Rational = unwrap . BSLex.readSigned readExponential1

readExponential2_Float    :: ByteString -> Float
readExponential2_Float    = unwrap . BSLex.readSigned readExponential2
readExponential2_Double   :: ByteString -> Double
readExponential2_Double   = unwrap . BSLex.readSigned readExponential2
readExponential2_Rational :: ByteString -> Rational
readExponential2_Rational = unwrap . BSLex.readSigned readExponential2

readExponential3_Float    :: ByteString -> Float
readExponential3_Float    = unwrap . BSLex.readSigned readExponential3
readExponential3_Double   :: ByteString -> Double
readExponential3_Double   = unwrap . BSLex.readSigned readExponential3
readExponential3_Rational :: ByteString -> Rational
readExponential3_Rational = unwrap . BSLex.readSigned readExponential3

readExponential4_Float    :: ByteString -> Float
readExponential4_Float    = unwrap . BSLex.readSigned readExponential4
readExponential4_Double   :: ByteString -> Double
readExponential4_Double   = unwrap . BSLex.readSigned readExponential4

-- The versions currently used by bytestring-read
fractional_Float         :: ByteString -> Float
fractional_Float         = unwrap . BSRead.signed  BSRead.fractional
fractional_Double        :: ByteString -> Double
fractional_Double        = unwrap . BSRead.signed  BSRead.fractional
fractional_Rational      :: ByteString -> Rational
fractional_Rational      = unwrap . BSRead.signed  BSRead.fractional


----------------------------------------------------------------
----------------------------------------------------------------
-- A QuickCheck property. Test that for a number >= 0, converting it to
-- a string using show and then reading the value back with the function
-- being tested returns the original value. The functions being
-- tested only work on positive numbers, so we check the absolute
-- value of the value that QuickCheck generates for us.
--
-- N.B., this test will not work for Rational since it uses the
-- @i%d@ notation instead of the @d.dEi@ notation.
--
-- TODO: recheck these inputs which were known to have failed when using the previous definition of approximate equality: 32.68783, 1.3629411, 2.864905, 6.768945, 1.5903426, 1.100002e-6, 1.1000005e-6, 4.744303, 1.3196326, 1.1000013e-6, 5.840101, 1.5105892, 1.1000002e-6, 1.474132, 1.1000006e-6, 2.226601, 1.1000012e-6
prop_read_show_idempotent
    :: (Fractional a, Ord a, Show a) => (ByteString -> a) -> a -> Bool
prop_read_show_idempotent freader x =
    let px = abs x
    in  px =~= freader (BS8.pack $ show px)


(=~=) :: (Fractional a, Ord a) => a -> a -> Bool
(=~=) a b = a == b || abs (a - b) <= max (abs a) (abs b) * 1e20


runQuickCheckTests :: IO ()
runQuickCheckTests = do
    putStrLn "Checking BSLexOld.readDouble..."
    QC.quickCheck (prop_read_show_idempotent readDouble)
    --
    putStrLn "Checking BSRead.fractional..."
    QC.quickCheck (prop_read_show_idempotent fractional_Float)
    QC.quickCheck (prop_read_show_idempotent fractional_Double)
    --
    putStrLn "Checking readExponential1..."
    QC.quickCheck (prop_read_show_idempotent readExponential1_Float)
    QC.quickCheck (prop_read_show_idempotent readExponential1_Double)
    --
    putStrLn "Checking readExponential2..."
    QC.quickCheck (prop_read_show_idempotent readExponential2_Float)
    QC.quickCheck (prop_read_show_idempotent readExponential2_Double)
    --
    putStrLn "Checking readExponential3..."
    QC.quickCheck (prop_read_show_idempotent readExponential3_Float)
    QC.quickCheck (prop_read_show_idempotent readExponential3_Double)
    --
    putStrLn "Checking readExponential4..."
    QC.quickCheck (prop_read_show_idempotent readExponential4_Float)
    QC.quickCheck (prop_read_show_idempotent readExponential4_Double)

----------------------------------------------------------------

benches :: NFData a => String -> (ByteString -> a) -> [Benchmark]
benches n f =
    [ bench (n++"_short") (nf f short)
    , bench (n++"_long")  (nf f long)
    ]

short :: ByteString
short = BS8.pack "-2342395232123424.3424346343524e3"
-- Truncates to   -2.3423952321234243e18 :: Double
--                -2.3423952         e18 :: Float

long :: ByteString
long = BS8.pack "-234232345678976521345895325678987654321345678987654321345689643213595232123424.34243463435223456789321367899231808534492500740957389523850293482093852039587495203586329850238562834290374029844e3"
-- Truncates to -2.3423234567897652e80 :: Double
--              -Infinity              :: Float

-- BUG: variance is always severely inflated by outliers... need a more reliable benchmark.
runCriterionTests :: IO ()
runCriterionTests = defaultMain
    [ bgroup "bytestring-lexing:readDouble" $ concat
        [ benches "Double" readDouble
        ]
    , bgroup "bytestring-read:fractional" $ concat
        [ benches "Float"    fractional_Float
        , benches "Double"   fractional_Double
        , benches "Rational" fractional_Rational
        ]
    , bgroup "readExponential1" $ concat
        [ benches "Float"    readExponential1_Float
        , benches "Double"   readExponential1_Double
        , benches "Rational" readExponential1_Rational
        ]
    , bgroup "readExponential2" $ concat
        [ benches "Float"    readExponential2_Float
        , benches "Double"   readExponential2_Double
        , benches "Rational" readExponential2_Rational
        ]
    , bgroup "readExponential3" $ concat
        [ benches "Float"    readExponential3_Float
        , benches "Double"   readExponential3_Double
        , benches "Rational" readExponential3_Rational
        ]
    , bgroup "readExponential4" $ concat
        [ benches "Float"    readExponential4_Float
        , benches "Double"   readExponential4_Double
        ]
    ]

main :: IO ()
main = do
    putStrLn "Quickcheck tests."
    runQuickCheckTests
    putStrLn "Criterion tests."
    runCriterionTests

----------------------------------------------------------------
----------------------------------------------------------- fin.