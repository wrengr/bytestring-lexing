{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
----------------------------------------------------------------
--                                                    2015.06.02
-- |
-- Module      :  BenchReadExponential
-- Copyright   :  Copyright (c) 2015 wren gayle romano,
--                              2015 Hirotomo Moriwaki
-- License     :  BSD2
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  benchmark
-- Portability :  portable
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

-- Version 1 of trying to speed things up. Is 3~3.5x faster than the old Alex version, but still ~1.5x and ~3x slower than BSRead at Float/Double (on the short and long inputs, respectively; the nonlinearity is no doubt due to BSRead's hackery about dropping low-order bits). However, is ~1.3x and ~2x faster(!) than BSRead at Rational
readDecimal1 :: (Fractional a) => ByteString -> Maybe (a, ByteString)
{-# SPECIALIZE readDecimal1 ::
    ByteString -> Maybe (Float,    ByteString),
    ByteString -> Maybe (Double,   ByteString),
    ByteString -> Maybe (Rational, ByteString) #-}
readDecimal1 xs0 =
    case BSLex.readDecimal xs0 of -- BUG: defaults to Integer...
    Nothing -> Nothing
    Just (whole, xs1)
        | BS.null xs1 || 0x2E /= BSU.unsafeHead xs1 ->
            justPair (fromIntegral whole) xs1
        | otherwise ->
            case BSLex.readDecimal (BSU.unsafeTail xs1) of -- BUG: defaults to Integer...
            Nothing          -> justPair (fromIntegral whole) xs1
            Just (part, xs2) ->
                let base = 10 ^ (BS.length xs1 - 1 - BS.length xs2)
                    frac = fromIntegral whole + (fromIntegral part / base)
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
            case BSLex.readSigned BSLex.readDecimal (BSU.unsafeTail xs1) of -- BUG: defaults to Integer...
            Nothing       -> justPair f xs1
            Just (e, xs2) ->
                let f' = if e >= 0 then f * (10 ^ e) else f / (10 ^ abs e)
                in justPair f' xs2


----------------------------------------------------------------
----------------------------------------------------------------

unwrap :: Maybe (a, ByteString) -> a
{-# INLINE unwrap #-}
unwrap Nothing   = error "couldn't parse input"
unwrap (Just (n,xs))
    | BS.null xs = n
    | otherwise  = error "input not fully parsed"

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
-- Failures that've showed up over the years: 32.68783, 1.3629411
prop_read_show_idempotent
    :: (Fractional a, Ord a, Show a) => (ByteString -> a) -> a -> Bool
prop_read_show_idempotent freader x =
    let px = abs x
    in  abs (px - freader (BS8.pack $ show px)) < epsilon

-- TODO: choose a better epsilon
epsilon :: Fractional a => a
epsilon = 0.0000001


runQuickCheckTests :: IO ()
runQuickCheckTests = do
    putStrLn "Checking BSLexOld.readDouble..."
    QC.quickCheck (prop_read_show_idempotent readDouble)
    --
    putStrLn "Checking readExponential1..."
    QC.quickCheck (prop_read_show_idempotent readExponential1_Float)
    QC.quickCheck (prop_read_show_idempotent readExponential1_Double)
    --
    putStrLn "Checking BSRead.fractional..."
    QC.quickCheck (prop_read_show_idempotent fractional_Float)
    QC.quickCheck (prop_read_show_idempotent fractional_Double)

----------------------------------------------------------------

doBench :: NFData a => String -> (ByteString -> a) -> Benchmark
doBench n f = bgroup n
    [ bench "short" $ nf f short
    , bench "long"  $ nf f long
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
    [ doBench "bytestring-lexing:readDouble" readDouble
    , doBench "bytestring-read:fractional@Float"    fractional_Float
    , doBench "bytestring-read:fractional@Double"   fractional_Double
    , doBench "bytestring-read:fractional@Rational" fractional_Rational
    , doBench "readExponential1@Float"    readExponential1_Float
    , doBench "readExponential1@Double"   readExponential1_Double
    , doBench "readExponential1@Rational" readExponential1_Rational
    ]

main :: IO ()
main = do
    putStrLn "Quickcheck tests."
    runQuickCheckTests
    putStrLn "Criterion tests."
    runCriterionTests

----------------------------------------------------------------
----------------------------------------------------------- fin.