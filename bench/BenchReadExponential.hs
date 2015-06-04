{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# LANGUAGE ScopedTypeVariables #-}
----------------------------------------------------------------
--                                                    2015.06.04
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
import           Control.DeepSeq                         (NFData)
import           Data.ByteString                         (ByteString)
import qualified Data.ByteString                         as BS
import qualified Data.ByteString.Char8                   as BS8
import qualified Test.QuickCheck                         as QC
import qualified Data.ByteString.Read                    as BSRead
import qualified BenchReadExponential.Double             as BSLexOld
import qualified BenchReadExponential.NewImplementations as BSLexNew
import qualified Data.ByteString.Lex.Integral            as BSLex

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

-- The old version used by bytestring-lexing
readDouble               :: ByteString -> Double
readDouble               = unwrap . BSLexOld.readDouble

-- The new versions being compared
s_readExponential1  :: Fractional a => ByteString -> a
s_readExponential1  = unwrap . BSLex.readSigned BSLexNew.readExponential1
s_readExponential11 :: Fractional a => ByteString -> a
s_readExponential11 = unwrap . BSLex.readSigned BSLexNew.readExponential11
s_readExponential2  :: Fractional a => ByteString -> a
s_readExponential2  = unwrap . BSLex.readSigned BSLexNew.readExponential2
s_readExponential3  :: Fractional a => ByteString -> a
s_readExponential3  = unwrap . BSLex.readSigned BSLexNew.readExponential3
s_readExponential31 :: Fractional a => ByteString -> a
s_readExponential31 = unwrap . BSLex.readSigned BSLexNew.readExponential31
s_readExponential32 :: Fractional a => ByteString -> a
s_readExponential32 = unwrap . BSLex.readSigned BSLexNew.readExponential32
s_readExponential4  :: RealFloat a => ByteString -> a
s_readExponential4  = unwrap . BSLex.readSigned BSLexNew.readExponential4


limit :: RealFloat a => a -> Int
limit proxy = length (show (floatRadix proxy ^ floatDigits proxy))

float_s_readExponential41 :: ByteString -> Float
float_s_readExponential41 =
    unwrap . BSLex.readSigned (BSLexNew.readExponential41 (limit (undefined::Float)))

double_s_readExponential41 :: ByteString -> Double
double_s_readExponential41 =
    unwrap . BSLex.readSigned (BSLexNew.readExponential41 (limit (undefined::Double)))

rational_s_readExponential41 :: ByteString -> Rational
rational_s_readExponential41 =
    unwrap . BSLex.readSigned (\xs -> BSLexNew.readExponential41 (1 + BS.length xs) xs)

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
    QC.quickCheck (prop_read_show_idempotent $ atFloat  s_fractional)
    QC.quickCheck (prop_read_show_idempotent $ atDouble s_fractional)
    --
    putStrLn "Checking readExponential1..."
    QC.quickCheck (prop_read_show_idempotent $ atFloat  s_readExponential1)
    QC.quickCheck (prop_read_show_idempotent $ atDouble s_readExponential1)
    --
    putStrLn "Checking readExponential11..."
    QC.quickCheck (prop_read_show_idempotent $ atFloat  s_readExponential11)
    QC.quickCheck (prop_read_show_idempotent $ atDouble s_readExponential11)
    --
    putStrLn "Checking readExponential2..."
    QC.quickCheck (prop_read_show_idempotent $ atFloat  s_readExponential2)
    QC.quickCheck (prop_read_show_idempotent $ atDouble s_readExponential2)
    --
    putStrLn "Checking readExponential3..."
    QC.quickCheck (prop_read_show_idempotent $ atFloat  s_readExponential3)
    QC.quickCheck (prop_read_show_idempotent $ atDouble s_readExponential3)
    --
    putStrLn "Checking readExponential31..."
    QC.quickCheck (prop_read_show_idempotent $ atFloat  s_readExponential31)
    QC.quickCheck (prop_read_show_idempotent $ atDouble s_readExponential31)
    --
    putStrLn "Checking readExponential32..."
    QC.quickCheck (prop_read_show_idempotent $ atFloat  s_readExponential32)
    QC.quickCheck (prop_read_show_idempotent $ atDouble s_readExponential32)
    --
    putStrLn "Checking readExponential4..."
    QC.quickCheck (prop_read_show_idempotent $ atFloat  s_readExponential4)
    QC.quickCheck (prop_read_show_idempotent $ atDouble s_readExponential4)
    --
    putStrLn "Checking readExponential41..."
    QC.quickCheck (prop_read_show_idempotent $ float_s_readExponential41)
    QC.quickCheck (prop_read_show_idempotent $ double_s_readExponential41)

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
        [ benches "Float"    $ atFloat    s_fractional
        , benches "Double"   $ atDouble   s_fractional
        , benches "Rational" $ atRational s_fractional
        ]
    {-
    , bgroup "readExponential1" $ concat
        [ benches "Float"    $ atFloat    s_readExponential1
        , benches "Double"   $ atDouble   s_readExponential1
        , benches "Rational" $ atRational s_readExponential1
        ]
    -}
    , bgroup "readExponential11" $ concat
        [ benches "Float"    $ atFloat    s_readExponential11
        , benches "Double"   $ atDouble   s_readExponential11
        , benches "Rational" $ atRational s_readExponential11
        ]
    {-
    , bgroup "readExponential2" $ concat
        [ benches "Float"    $ atFloat    s_readExponential2
        , benches "Double"   $ atDouble   s_readExponential2
        , benches "Rational" $ atRational s_readExponential2
        ]
    , bgroup "readExponential3" $ concat
        [ benches "Float"    $ atFloat    s_readExponential3
        , benches "Double"   $ atDouble   s_readExponential3
        , benches "Rational" $ atRational s_readExponential3
        ]
    , bgroup "readExponential31" $ concat
        [ benches "Float"    $ atFloat    s_readExponential31
        , benches "Double"   $ atDouble   s_readExponential31
        , benches "Rational" $ atRational s_readExponential31
        ]
    , bgroup "readExponential32" $ concat
        [ benches "Float"    $ atFloat    s_readExponential32
        , benches "Double"   $ atDouble   s_readExponential32
        , benches "Rational" $ atRational s_readExponential32
        ]
    , bgroup "readExponential4" $ concat
        [ benches "Float"    $ atFloat    s_readExponential4
        , benches "Double"   $ atDouble   s_readExponential4
        ]
    -}
    , bgroup "readExponential41" $ concat
        [ benches "Float"    $ float_s_readExponential41
        , benches "Double"   $ double_s_readExponential41
        , benches "Rational" $ rational_s_readExponential41
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