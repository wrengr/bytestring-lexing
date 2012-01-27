{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# LANGUAGE OverloadedStrings, MagicHash, BangPatterns #-}
----------------------------------------------------------------
--                                                    2012.01.26
-- |
-- Copyright     : Erik de Castro Lopo <erikd@mega-nerd.com>
-- License       : BSD3
--
-- Originally a program to QuickCheck and benchmark a function used
-- in the Warp web server and elsewhere to read the Content-Length
-- field of HTTP headers. This version modified for testing the
-- local package.
--
-- Compile and run as:
--    @ghc -O2 --make readDecimal.hs -o readDecimal && ./readDecimal@
----------------------------------------------------------------

import Criterion.Main
import Data.ByteString (ByteString)
import Data.Int        (Int64)
import Data.Maybe      (fromMaybe)

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Char             as C
import qualified Numeric               as N
import qualified Test.QuickCheck       as QC

-- ReadInt is a module internal to Warp.
----import qualified ReadInt as RI

import qualified Data.ByteString.Lex.Integral as BSLex (readDecimal)

import GHC.Prim
import GHC.Types
----------------------------------------------------------------

-- This is the absolute mimimal solution. It will return garbage
-- if the input string contains anything other than ASCII digits.
readIntOrig :: ByteString -> Integer
readIntOrig = BS.foldl' (\x w -> x * 10 + fromIntegral w - 48) 0


-- Using Numeric.readDec which works on String, so the ByteString
-- has to be unpacked first.
readDec :: ByteString -> Integer
readDec s =
    case N.readDec (BS8.unpack s) of
    []      -> 0
    (x,_):_ -> x


-- Use ByteString's readInteger function.
readIntegerBS :: ByteString -> Integer
readIntegerBS = fst . fromMaybe (0,"") . BS8.readInteger


-- No checking for non-digits. Will overflow at 2^31 on 32 bit CPUs.
readIntRaw :: ByteString -> Int
readIntRaw = BS8.foldl' (\i c -> i * 10 + C.digitToInt c) 0

----------------------------------------------------------------
-- The best solution Erik found.
readIntTC :: Integral a => ByteString -> a
readIntTC
    = fromIntegral
    . BS8.foldl' (\i c -> i * 10 + C.digitToInt c) 0 
    . BS8.takeWhile C.isDigit

-- Three specialisations of readIntTC.
readInt :: ByteString -> Int
readInt = readIntTC

readInt64 :: ByteString -> Int64
readInt64 = readIntTC

readInteger :: ByteString -> Integer
readInteger = readIntTC

----------------------------------------------------------------
-- MagicHash version suggested by Vincent Hanquez.
readIntMH :: Integral a => ByteString -> a
readIntMH = fromIntegral . ireadInt64MH
    where
    ireadInt64MH :: ByteString -> Int64
    ireadInt64MH
        = BS8.foldl' (\i c -> i * 10 + fromIntegral (mhDigitToInt c)) 0
        . BS8.takeWhile C.isDigit

readInt64MH :: ByteString -> Int64
readInt64MH = readIntMH

readIntegerMH :: ByteString -> Integer
readIntegerMH = readIntMH

data Table = Table !Addr#

mhDigitToInt :: Char -> Int
mhDigitToInt (C# i) = I# (word2Int# (indexWord8OffAddr# addr (ord# i)))
    where
    !(Table addr) = table
    table :: Table
    table = Table
        "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

-- -> -- Fix a syntax highlighting bug in jEdit.

-- This is the one Warp actually uses. It's essentially the same as readInt64MH
----readIntWarp :: ByteString -> Integer
----readIntWarp s = fromIntegral $ RI.readInt64 s

----------------------------------------------------------------
readDecimalInt_      :: ByteString -> Int
readDecimalInt_      = fst . fromMaybe (0,"") . BSLex.readDecimal

readDecimalInt64_    :: ByteString -> Int64
readDecimalInt64_    = fst . fromMaybe (0,"") . BSLex.readDecimal

readDecimalInt64_2   :: ByteString -> Int64
readDecimalInt64_2   = fromIntegral . readDecimalInt_

readDecimalInteger_  :: ByteString -> Integer
readDecimalInteger_  = fst . fromMaybe (0,"") . BSLex.readDecimal

readDecimalInteger_2 :: ByteString -> Integer
readDecimalInteger_2 = fromIntegral . readDecimalInt_


----------------------------------------------------------------
-- A QuickCheck property. Test that for a number >= 0, converting it to
-- a string using show and then reading the value back with the function
-- under test returns the original value.
-- The functions under test only work on Natural numbers (the Conent-Length
-- field in a HTTP header is always >= 0) so we check the absolute value of
-- the value that QuickCheck generates for us.
prop_read_show_idempotent :: Integral a => (ByteString -> a) -> a -> Bool
prop_read_show_idempotent freader x =
    let px = abs x
    in px == freader (BS8.pack $ show px)


runQuickCheckTests :: IO ()
runQuickCheckTests = do
    putStrLn "Checking readInt..."
    QC.quickCheck (prop_read_show_idempotent readInt)
    putStrLn "Checking readInt64..."
    QC.quickCheck (prop_read_show_idempotent readInt64)
    putStrLn "Checking readInteger..."
    QC.quickCheck (prop_read_show_idempotent readInteger)
    putStrLn "Checking readInt64MH..."
    QC.quickCheck (prop_read_show_idempotent readInt64MH)
    putStrLn "Checking readIntegerMH..."
    QC.quickCheck (prop_read_show_idempotent readIntegerMH)
    ----putStrLn "Checking readIntWarp..."
    ----QC.quickCheck (prop_read_show_idempotent readIntWarp)
    putStrLn "Checking readDecimalInt_..."
    QC.quickCheck (prop_read_show_idempotent readDecimalInt_)
    putStrLn "Checking readDecimalInt64_..."
    QC.quickCheck (prop_read_show_idempotent readDecimalInt64_)
    putStrLn "Checking readDecimalInt64_2..."
    QC.quickCheck (prop_read_show_idempotent readDecimalInt64_2)
    putStrLn "Checking readDecimalInteger_..."
    QC.quickCheck (prop_read_show_idempotent readDecimalInteger_)
    putStrLn "Checking readDecimalInteger_2..."
    QC.quickCheck (prop_read_show_idempotent readDecimalInteger_2)


runCriterionTests :: ByteString -> IO ()
runCriterionTests number =
    defaultMain
       [ bench "readIntOrig"   $ nf readIntOrig number
       , bench "readDec"       $ nf readDec number
       , bench "readIntegerBS" $ nf readIntegerBS number
       , bench "readRaw"       $ nf readIntRaw number
       , bench "readInt"       $ nf readInt number
       , bench "readInt64"     $ nf readInt64 number
       , bench "readInteger"   $ nf readInteger number
       , bench "readInt64MH"   $ nf readInt64MH number
       , bench "readIntegerMH" $ nf readIntegerMH number
       ---- , bench "readIntWarp"   $ nf readIntWarp number
       , bench "readDecimalInt_" $ nf readDecimalInt_ number
       , bench "readDecimalInt64_" $ nf readDecimalInt64_ number
       , bench "readDecimalInt64_2" $ nf readDecimalInt64_2 number
       , bench "readDecimalInteger_" $ nf readDecimalInteger_ number
       , bench "readDecimalInteger_2" $ nf readDecimalInteger_2 number
       ]

main :: IO ()
main = do
    putStrLn "Quickcheck tests."
    runQuickCheckTests
    putStrLn "Criterion tests."
    runCriterionTests "1234567898765432178979128361238162386182"

