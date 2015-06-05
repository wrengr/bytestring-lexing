{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# LANGUAGE CPP, ScopedTypeVariables #-}
----------------------------------------------------------------
--                                                    2015.06.05
-- |
-- Module      :  BenchReadExponential
-- Copyright   :  Copyright (c) 2015 wren gayle romano,
--                              2015 Hirotomo Moriwaki
-- License     :  BSD2
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  benchmark
-- Portability :  CPP, ScopedTypeVariables
--
-- Benchmark the speed of parsing floating point numbers. This
-- benchmark originally came from @bytestring-read@ version 0.3.0.
----------------------------------------------------------------
module BenchReadExponential (main) where

import           Criterion.Main
import           Control.DeepSeq                         (NFData)
import           Data.Proxy                              (Proxy(Proxy))
import           Data.ByteString                         (ByteString)
import qualified Data.ByteString                         as BS
import qualified Data.ByteString.Char8                   as BS8
import qualified Test.QuickCheck                         as QC
import qualified Data.ByteString.Read                    as BSRead
import qualified BenchReadExponential.Double             as BSLexOld
import qualified BenchReadExponential.NewImplementations as BSLexTest
import qualified Data.ByteString.Lex.Fractional          as BSLex

-- N.B., ./dist/build/autogen/cabal_macros.h defines the VERSION_foo macros as well as the MIN_VERSION_foo(x,y,z) macros

----------------------------------------------------------------
----------------------------------------------------------------

unwrap :: Maybe (a, ByteString) -> a
{-# INLINE unwrap #-}
unwrap Nothing   = error "couldn't parse input at all"
unwrap (Just (n,xs))
    | BS.null xs = n
    | otherwise  = error ("input not fully parsed: " ++ BS8.unpack xs)

-- Benchmarking indicates that using @atFoo@ does not affect performance compared to using top-level bindings to monomorphize the outputs.
atFloat :: (a -> Float) -> a -> Float
atFloat = id

atDouble :: (a -> Double) -> a -> Double
atDouble = id

atRational :: (a -> Rational) -> a -> Rational
atRational = id
    
----------------------------------------------------------------
-- The version currently used by bytestring-read
s_fractional             :: (BSRead.ReadFractional a) => ByteString -> a
s_fractional             = unwrap . BSRead.signed BSRead.fractional

-- The old version used by bytestring-lexing <= 0.4.3.3
readDouble               :: ByteString -> Double
readDouble               = unwrap . BSLexOld.readDouble

-- The versions used by the current version of bytestring-lexing
s_readExpCurrent        :: Fractional a => ByteString -> a
s_readExpCurrent        = unwrap . BSLex.readSigned BSLex.readExponential

float_s_readExpLimCurrent :: ByteString -> Float
float_s_readExpLimCurrent =
    unwrap . BSLex.readSigned
        (BSLex.readExponentialLimited
            (BSLex.decimalPrecision (Proxy::Proxy Float)))
double_s_readExpLimCurrent :: ByteString -> Double
double_s_readExpLimCurrent =
    unwrap . BSLex.readSigned
        (BSLex.readExponentialLimited
            (BSLex.decimalPrecision (Proxy::Proxy Double)))

s_readExpLimInftyCurrent :: Fractional a => ByteString -> a
s_readExpLimInftyCurrent =
    unwrap . BSLex.readSigned
        (\xs -> BSLex.readExponentialLimited (1 + BS.length xs) xs)

-- The test versions being compared
s_readExpTest1  :: Fractional a => ByteString -> a
s_readExpTest1  = unwrap . BSLex.readSigned BSLexTest.readExponential1
s_readExpTest11 :: Fractional a => ByteString -> a
s_readExpTest11 = unwrap . BSLex.readSigned BSLexTest.readExponential11
s_readExpTest12 :: Fractional a => ByteString -> a
s_readExpTest12 = unwrap . BSLex.readSigned BSLexTest.readExponential12
s_readExpTest2  :: Fractional a => ByteString -> a
s_readExpTest2  = unwrap . BSLex.readSigned BSLexTest.readExponential2
s_readExpTest3  :: Fractional a => ByteString -> a
s_readExpTest3  = unwrap . BSLex.readSigned BSLexTest.readExponential3
s_readExpTest31 :: Fractional a => ByteString -> a
s_readExpTest31 = unwrap . BSLex.readSigned BSLexTest.readExponential31
s_readExpTest32 :: Fractional a => ByteString -> a
s_readExpTest32 = unwrap . BSLex.readSigned BSLexTest.readExponential32
s_readExpTest4  :: RealFloat a => ByteString -> a
s_readExpTest4  = unwrap . BSLex.readSigned BSLexTest.readExponential4


limit :: RealFloat a => a -> Int
limit proxy = length (show (floatRadix proxy ^ floatDigits proxy))

float_s_readExpTest41 :: ByteString -> Float
float_s_readExpTest41 =
    unwrap . BSLex.readSigned (BSLexTest.readExponential41 (limit (undefined::Float)))
double_s_readExpTest41 :: ByteString -> Double
double_s_readExpTest41 =
    unwrap . BSLex.readSigned (BSLexTest.readExponential41 (limit (undefined::Double)))
rational_s_readExpTest41 :: ByteString -> Rational
rational_s_readExpTest41 =
    unwrap . BSLex.readSigned (\xs -> BSLexTest.readExponential41 (1 + BS.length xs) xs)

float_s_readExpTest42 :: ByteString -> Float
float_s_readExpTest42 =
    unwrap . BSLex.readSigned (BSLexTest.readExponential42 (limit (undefined::Float)))
double_s_readExpTest42 :: ByteString -> Double
double_s_readExpTest42 =
    unwrap . BSLex.readSigned (BSLexTest.readExponential42 (limit (undefined::Double)))
rational_s_readExpTest42 :: ByteString -> Rational
rational_s_readExpTest42 =
    unwrap . BSLex.readSigned (\xs -> BSLexTest.readExponential42 (1 + BS.length xs) xs)

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
    putStrLn "Checking BSLex.readExponential..."
    QC.quickCheck (prop_read_show_idempotent $ atFloat  s_readExpCurrent)
    QC.quickCheck (prop_read_show_idempotent $ atDouble s_readExpCurrent)
    --
    putStrLn "Checking BSLex.readExponentialLimited@decimalPrecision..."
    QC.quickCheck (prop_read_show_idempotent $ float_s_readExpLimCurrent)
    QC.quickCheck (prop_read_show_idempotent $ double_s_readExpLimCurrent)
    --
    putStrLn "Checking BSLex.readExponentialLimited@infinity..."
    QC.quickCheck (prop_read_show_idempotent $ atFloat s_readExpLimInftyCurrent)
    QC.quickCheck (prop_read_show_idempotent $ atDouble s_readExpLimInftyCurrent)
    --
    putStrLn "Checking BSRead.fractional..."
    QC.quickCheck (prop_read_show_idempotent $ atFloat  s_fractional)
    QC.quickCheck (prop_read_show_idempotent $ atDouble s_fractional)
    --
    putStrLn "Checking readExponential1..."
    QC.quickCheck (prop_read_show_idempotent $ atFloat  s_readExpTest1)
    QC.quickCheck (prop_read_show_idempotent $ atDouble s_readExpTest1)
    --
    putStrLn "Checking readExponential11..."
    QC.quickCheck (prop_read_show_idempotent $ atFloat  s_readExpTest11)
    QC.quickCheck (prop_read_show_idempotent $ atDouble s_readExpTest11)
    --
    putStrLn "Checking readExponential12..."
    QC.quickCheck (prop_read_show_idempotent $ atFloat  s_readExpTest12)
    QC.quickCheck (prop_read_show_idempotent $ atDouble s_readExpTest12)
    --
    putStrLn "Checking readExponential2..."
    QC.quickCheck (prop_read_show_idempotent $ atFloat  s_readExpTest2)
    QC.quickCheck (prop_read_show_idempotent $ atDouble s_readExpTest2)
    --
    putStrLn "Checking readExponential3..."
    QC.quickCheck (prop_read_show_idempotent $ atFloat  s_readExpTest3)
    QC.quickCheck (prop_read_show_idempotent $ atDouble s_readExpTest3)
    --
    putStrLn "Checking readExponential31..."
    QC.quickCheck (prop_read_show_idempotent $ atFloat  s_readExpTest31)
    QC.quickCheck (prop_read_show_idempotent $ atDouble s_readExpTest31)
    --
    putStrLn "Checking readExponential32..."
    QC.quickCheck (prop_read_show_idempotent $ atFloat  s_readExpTest32)
    QC.quickCheck (prop_read_show_idempotent $ atDouble s_readExpTest32)
    --
    putStrLn "Checking readExponential4..."
    QC.quickCheck (prop_read_show_idempotent $ atFloat  s_readExpTest4)
    QC.quickCheck (prop_read_show_idempotent $ atDouble s_readExpTest4)
    --
    putStrLn "Checking readExponential41..."
    QC.quickCheck (prop_read_show_idempotent $ float_s_readExpTest41)
    QC.quickCheck (prop_read_show_idempotent $ double_s_readExpTest41)
    --
    putStrLn "Checking readExponential42..."
    QC.quickCheck (prop_read_show_idempotent $ float_s_readExpTest42)
    QC.quickCheck (prop_read_show_idempotent $ double_s_readExpTest42)

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
    [ bgroup "bytestring-lexing-0.4.3.3:readDouble" $ concat
        [ benches "Double" readDouble
        ]
    , bgroup ("bytestring-read-" ++ VERSION_bytestring_read ++ ":fractional") $ concat
        [ benches "Float"    $ atFloat    s_fractional
        , benches "Double"   $ atDouble   s_fractional
        , benches "Rational" $ atRational s_fractional
        ]
    , bgroup ("bytestring-lexing-" ++ VERSION_bytestring_lexing ++ ":readExponential") $ concat
        [ benches "Float"    $ atFloat    s_readExpCurrent
        , benches "Double"   $ atDouble   s_readExpCurrent
        , benches "Rational" $ atRational s_readExpCurrent
        ]
    , bgroup ("bytestring-lexing-" ++ VERSION_bytestring_lexing ++ ":readExpLim@inherent") $ concat
        [ benches "Float"    $ float_s_readExpLimCurrent
        , benches "Double"   $ double_s_readExpLimCurrent
        ]
    , bgroup ("bytestring-lexing-" ++ VERSION_bytestring_lexing ++ ":readExpLim@infinity") $ concat
        [ benches "Float"    $ atFloat    s_readExpLimInftyCurrent
        , benches "Double"   $ atDouble   s_readExpLimInftyCurrent
        , benches "Rational" $ atRational s_readExpLimInftyCurrent
        ]
    {-
    , bgroup "readExponential1" $ concat
        [ benches "Float"    $ atFloat    s_readExpTest1
        , benches "Double"   $ atDouble   s_readExpTest1
        , benches "Rational" $ atRational s_readExpTest1
        ]
    , bgroup "readExponential11" $ concat
        [ benches "Float"    $ atFloat    s_readExpTest11
        , benches "Double"   $ atDouble   s_readExpTest11
        , benches "Rational" $ atRational s_readExpTest11
        ]
    , bgroup "readExponential12" $ concat
        [ benches "Float"    $ atFloat    s_readExpTest12
        , benches "Double"   $ atDouble   s_readExpTest12
        , benches "Rational" $ atRational s_readExpTest12
        ]
    , bgroup "readExponential2" $ concat
        [ benches "Float"    $ atFloat    s_readExpTest2
        , benches "Double"   $ atDouble   s_readExpTest2
        , benches "Rational" $ atRational s_readExpTest2
        ]
    , bgroup "readExponential3" $ concat
        [ benches "Float"    $ atFloat    s_readExpTest3
        , benches "Double"   $ atDouble   s_readExpTest3
        , benches "Rational" $ atRational s_readExpTest3
        ]
    , bgroup "readExponential31" $ concat
        [ benches "Float"    $ atFloat    s_readExpTest31
        , benches "Double"   $ atDouble   s_readExpTest31
        , benches "Rational" $ atRational s_readExpTest31
        ]
    , bgroup "readExponential32" $ concat
        [ benches "Float"    $ atFloat    s_readExpTest32
        , benches "Double"   $ atDouble   s_readExpTest32
        , benches "Rational" $ atRational s_readExpTest32
        ]
    , bgroup "readExponential4" $ concat
        [ benches "Float"    $ atFloat    s_readExpTest4
        , benches "Double"   $ atDouble   s_readExpTest4
        ]
    , bgroup "readExponential41" $ concat
        [ benches "Float"    $ float_s_readExpTest41
        , benches "Double"   $ double_s_readExpTest41
        , benches "Rational" $ rational_s_readExpTest41
        ]
    , bgroup "readExponential42" $ concat
        [ benches "Float"    $ float_s_readExpTest42
        , benches "Double"   $ double_s_readExpTest42
        , benches "Rational" $ rational_s_readExpTest42
        ]
    -}
    ]

main :: IO ()
main = do
    putStrLn "Quickcheck tests."
    runQuickCheckTests
    putStrLn "Criterion tests."
    runCriterionTests

----------------------------------------------------------------
----------------------------------------------------------- fin.