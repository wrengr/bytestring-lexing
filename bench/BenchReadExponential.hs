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
import qualified Data.ByteString.Char8          as BS8
import qualified Test.QuickCheck                as QC
import qualified Data.ByteString.Lex.Fractional as BSLex
import qualified Data.ByteString.Lex.Double     as BSLex
import qualified Data.ByteString.Read           as BSRead

----------------------------------------------------------------
----------------------------------------------------------------

unwrap :: Maybe (a, ByteString) -> a
{-# INLINE unwrap #-}
unwrap Nothing   = error "couldn't parse input"
unwrap (Just (n,xs))
    | BS.null xs = n
    | otherwise  = error "input not fully parsed"

----------------------------------------------------------------
-- The versions currently used by my library
-- Could also try comparing Float and Rational...

readDouble_Double        :: ByteString -> Double
readDouble_Double        = unwrap . BSLex.readDouble

readExponential_Double   :: ByteString -> Double
readExponential_Double   = unwrap . BSLex.readSigned BSLex.readExponential

-- The versions currently used by bytestring-read
fractional_Double        :: ByteString -> Double
fractional_Double        = unwrap . BSRead.signed  BSRead.fractional


----------------------------------------------------------------
----------------------------------------------------------------
-- A QuickCheck property. Test that for a number >= 0, converting it to
-- a string using show and then reading the value back with the function
-- being tested returns the original value. The functions being
-- tested only work on positive numbers, so we check the absolute
-- value of the value that QuickCheck generates for us.
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
    putStrLn "Checking BSLex.readDouble..."
    QC.quickCheck (prop_read_show_idempotent readDouble_Double)
    --
    putStrLn "Checking BSLex.readExponential..."
    QC.quickCheck (prop_read_show_idempotent readExponential_Double)
    --
    putStrLn "Checking BSRead.fractional..."
    QC.quickCheck (prop_read_show_idempotent fractional_Double)

----------------------------------------------------------------

doBench :: NFData a => String -> (ByteString -> a) -> Benchmark
doBench n f = bgroup n
    [ bench "short" $ nf f short
    , bench "long"  $ nf f long
    ]
    
short :: ByteString
short = BS8.pack "-2342395232123424.3424346343524e3"

long :: ByteString
long = BS8.pack "-234232345678976521345895325678987654321345678987654321345689643213595232123424.34243463435223456789321367899231808534492500740957389523850293482093852039587495203586329850238562834290374029844e3"


runCriterionTests :: IO ()
runCriterionTests = defaultMain
    [ doBench "bytestring-lexing:readDouble" readDouble_Double
    , doBench "bytestring-lexing:readExponential@Double"   readExponential_Double
    , doBench "bytestring-read:fractional@Double" fractional_Double
    ]

main :: IO ()
main = do
    putStrLn "Quickcheck tests."
    runQuickCheckTests
    putStrLn "Criterion tests."
    runCriterionTests

----------------------------------------------------------------
----------------------------------------------------------- fin.