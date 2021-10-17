{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# LANGUAGE OverloadedStrings, MagicHash, BangPatterns #-}
----------------------------------------------------------------
--                                                    2021.10.17
-- |
-- Module      :  BenchReadDecimal
-- Copyright   :  Copyright (c) 2010--2021 wren gayle romano,
--                              2012 Erik de Castro Lopo <erikd@mega-nerd.com>
-- License     :  BSD2
-- Maintainer  :  wren@cpan.org
-- Stability   :  benchmark
-- Portability :  portable
--
-- Originally a program to QuickCheck and benchmark a function used
-- in the Warp web server and elsewhere to read the Content-Length
-- field of HTTP headers. This version modified for testing the
-- local package.
----------------------------------------------------------------
module BenchReadDecimal (main) where

import Criterion.Main
import Data.ByteString (ByteString)
import Data.Int        (Int64)
import Data.Maybe      (fromMaybe)

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Unsafe   as BSU
import qualified Data.Char             as C
import qualified Numeric               as N
import qualified Test.QuickCheck       as QC

-- ReadInt is a module internal to Warp.
----import qualified ReadInt as RI

import qualified Data.ByteString.Lex.Integral as BSLex

import GHC.Prim
import GHC.Types
import GHC.Word
----------------------------------------------------------------

unwrap :: Num a => Maybe (a, ByteString) -> a
{-# INLINE unwrap #-}
unwrap = fst . fromMaybe (0,"")


-- This is the absolute mimimal solution. It will return garbage
-- if the input string contains anything other than ASCII digits.
bsfoldl_Integer :: ByteString -> Integer
bsfoldl_Integer = BS.foldl' (\x w -> x * 10 + fromIntegral w - 48) 0

-- No checking for non-digits. Will overflow at 2^31 on 32 bit CPUs.
bs8foldl_Int :: ByteString -> Int
bs8foldl_Int = BS8.foldl' (\i c -> i * 10 + C.digitToInt c) 0

-- Using Numeric.readDec which works on String, so the ByteString
-- has to be unpacked first.
numericReadDec_Integer :: ByteString -> Integer
numericReadDec_Integer s =
    case N.readDec (BS8.unpack s) of
    []      -> 0
    (x,_):_ -> x

-- Use ByteString's readInt function.
bs8readInt :: ByteString -> Int
bs8readInt = unwrap . BS8.readInt

-- Use ByteString's readInteger function.
bs8readInteger :: ByteString -> Integer
bs8readInteger = unwrap . BS8.readInteger

----------------------------------------------------------------
-- The best solution Erik found.
readIntegralTC :: Integral a => ByteString -> a
readIntegralTC
    = fromIntegral
    . BS8.foldl' (\i c -> i * 10 + C.digitToInt c) 0
    . BS8.takeWhile C.isDigit

readIntTC :: ByteString -> Int
readIntTC = readIntegralTC

readInt64TC :: ByteString -> Int64
readInt64TC = readIntegralTC

readIntegerTC :: ByteString -> Integer
readIntegerTC = readIntegralTC

----------------------------------------------------------------
-- MagicHash version suggested by Vincent Hanquez.
readIntegralMH :: Integral a => ByteString -> a
readIntegralMH = fromIntegral . ireadInt64MH
    where
    ireadInt64MH :: ByteString -> Int64
    ireadInt64MH
        = BS8.foldl' (\i c -> i * 10 + fromIntegral (mhDigitToInt c)) 0
        . BS8.takeWhile C.isDigit

readIntMH :: ByteString -> Int
readIntMH = readIntegralMH

readInt64MH :: ByteString -> Int64
readInt64MH = readIntegralMH

readIntegerMH :: ByteString -> Integer
readIntegerMH = readIntegralMH

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

----------------------------------------------------------------
-- A faster MagicHash version by Christoph Breitkopf
{-
readIntegralMHFast :: Integral a => ByteString -> a
readIntegralMHFast s = go 0 0 (BS8.length s) s
    where
    go :: Integral a => a -> Int -> Int -> ByteString -> a
    go n i len bs
        | n `seq` i `seq` len `seq` bs `seq` False = undefined
        | i >= len  = n
        | v < 10    = go (10 * n + v) (i+1) len bs
        | otherwise = n
        where
        v = fromIntegral (mhDigitToIntFast (BS8.index bs i))

mhDigitToIntFast :: Char -> Int
mhDigitToIntFast (C# i) = I# (word2Int# (indexWord8OffAddr# addr (ord# i)))
    where
    !(Table addr) = table
    table :: Table
    table = Table
        "\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\
        \\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\
        \\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\
        \\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x7f\x7f\x7f\x7f\x7f\x7f\
        \\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\
        \\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\
        \\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\
        \\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\
        \\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\
        \\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\
        \\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\
        \\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\
        \\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\
        \\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\
        \\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\
        \\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f"#
-- -> -- Fix a syntax highlighting bug in jEdit.

readIntMHFast :: ByteString -> Int
readIntMHFast = readIntegralMHFast

readInt64MHFast :: ByteString -> Int64
readInt64MHFast = readIntegralMHFast

readIntegerMHFast :: ByteString -> Integer
readIntegerMHFast = readIntegralMHFast
-}

-- Crank on it just a bit more
readIntegralMHFast :: Integral a => ByteString -> a
readIntegralMHFast s = go 0 0 s
    where
    len = BS8.length s

    go :: Integral a => a -> Int -> ByteString -> a
    go n i bs
        | n `seq` i `seq` bs `seq` False = undefined
        | i >= len  = n
        | v < 10    = go (10 * n + v) (i+1) bs
        | otherwise = n
        where
        v = fromIntegral (mhDigitToIntFast (BSU.unsafeIndex bs i))

mhDigitToIntFast :: Word8 -> Int
{-# INLINE mhDigitToIntFast #-}
mhDigitToIntFast (W8# i) = I# (indexInt8OffAddr# addr (word2Int# i))
    where
    {-# NOINLINE addr #-}
    !(Table addr) = table
    table :: Table
    table = Table
        "\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\
        \\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\
        \\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\
        \\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x7f\x7f\x7f\x7f\x7f\x7f\
        \\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\
        \\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\
        \\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\
        \\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\
        \\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\
        \\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\
        \\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\
        \\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\
        \\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\
        \\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\
        \\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\
        \\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f"#
-- -> -- Fix a syntax highlighting bug in jEdit.

readIntMHFast :: ByteString -> Int
readIntMHFast = readIntegralMHFast

readInt64MHFast :: ByteString -> Int64
readInt64MHFast = readIntegralMHFast

readIntegerMHFast :: ByteString -> Integer
readIntegerMHFast = readIntegralMHFast

----------------------------------------------------------------
-- This is the one Warp actually uses. It's essentially the same as readInt64MH
----readIntWarp :: ByteString -> Integer
----readIntWarp s = fromIntegral $ RI.readInt64 s

----------------------------------------------------------------
readDecimalOrig :: (Integral a) => ByteString -> Maybe (a, ByteString)
readDecimalOrig = start
    where
    -- This implementation is near verbatim from
    -- bytestring-0.9.1.7:Data.ByteString.Char8.readInt. We do
    -- remove the superstrictness by lifting the 'Just' so it can
    -- be returned after seeing the first byte. Do beware of the
    -- scope of 'fromIntegral', we want to avoid unnecessary
    -- 'Integral' operations and do as much as possible in 'Word8'.
    start xs
        | BS.null xs = Nothing
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    Just $ loop (fromIntegral (w - 0x30)) (BSU.unsafeTail xs)
              | otherwise -> Nothing

    loop n xs
        | n `seq` xs `seq` False = undefined -- for strictness analysis
        | BS.null xs = (n, BS.empty)         -- not @xs@, to help GC
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    loop (n * 10 + fromIntegral (w - 0x30)) (BSU.unsafeTail xs)
              | otherwise -> (n,xs)

readDecimalOrig_Int      :: ByteString -> Int
readDecimalOrig_Int      = unwrap . readDecimalOrig

readDecimalOrig_Int64    :: ByteString -> Int64
readDecimalOrig_Int64    = unwrap . readDecimalOrig

readDecimalHack_Int64    :: ByteString -> Int64
readDecimalHack_Int64    = fromIntegral . readDecimalOrig_Int

readDecimalOrig_Integer  :: ByteString -> Integer
readDecimalOrig_Integer  = unwrap . readDecimalOrig

readDecimalHack_Integer  :: ByteString -> Integer
readDecimalHack_Integer  = fromIntegral . readDecimalOrig_Int


----------------------------------------------------------------
-- This splits the difference between the slow but correct
-- 'readDecimalOrig_Int64' (840ns) and the fast but incorrect
-- 'readDecimalHack_Int64' (175ns) at about 475ns on my machine.
-- N.B., passing a parameter to track our position in the group
-- reduces performance down to the level of 'bsfoldl_Integer' and
-- 'bs8readInteger'; so the code duplication of unrolling the loop
-- seems necessary for this approach.
readDecimalUnrolled :: Integral a => ByteString -> Maybe (a, ByteString)
readDecimalUnrolled = start
    where
    start xs
        | BS.null xs = Nothing
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    Just $ loop0 (fromIntegral(w-0x30)) (BSU.unsafeTail xs)
              | otherwise -> Nothing

    loop0 :: Integral a => a -> ByteString -> (a, ByteString)
    loop0 m xs
        | m `seq` xs `seq` False = undefined
        | BS.null xs = (m, BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    loop1 m (fromIntegral(w-0x30)) (BSU.unsafeTail xs)
              | otherwise -> (m, xs)

    loop1, loop2, loop3, loop4, loop5, loop6, loop7, loop8 :: Integral a => a -> Int -> ByteString -> (a, ByteString)
    loop1 m n xs
        | m `seq` n `seq` xs `seq` False = undefined
        | BS.null xs = (m*10 + fromIntegral n, BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    loop2 m (n*10 + fromIntegral(w-0x30)) (BSU.unsafeTail xs)
              | otherwise -> (m*10 + fromIntegral n, xs)
    loop2 m n xs
        | m `seq` n `seq` xs `seq` False = undefined
        | BS.null xs = (m*100 + fromIntegral n, BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    loop3 m (n*10 + fromIntegral(w-0x30)) (BSU.unsafeTail xs)
              | otherwise -> (m*100 + fromIntegral n, xs)
    loop3 m n xs
        | m `seq` n `seq` xs `seq` False = undefined
        | BS.null xs = (m*1000 + fromIntegral n, BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    loop4 m (n*10 + fromIntegral(w-0x30)) (BSU.unsafeTail xs)
              | otherwise -> (m*1000 + fromIntegral n, xs)
    loop4 m n xs
        | m `seq` n `seq` xs `seq` False = undefined
        | BS.null xs = (m*10000 + fromIntegral n, BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    loop5 m (n*10 + fromIntegral(w-0x30)) (BSU.unsafeTail xs)
              | otherwise -> (m*10000 + fromIntegral n, xs)
    loop5 m n xs
        | m `seq` n `seq` xs `seq` False = undefined
        | BS.null xs = (m*100000 + fromIntegral n, BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    loop6 m (n*10 + fromIntegral(w-0x30)) (BSU.unsafeTail xs)
              | otherwise -> (m*100000 + fromIntegral n, xs)
    loop6 m n xs
        | m `seq` n `seq` xs `seq` False = undefined
        | BS.null xs = (m*1000000 + fromIntegral n, BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    loop7 m (n*10 + fromIntegral(w-0x30)) (BSU.unsafeTail xs)
              | otherwise -> (m*1000000 + fromIntegral n, xs)
    loop7 m n xs
        | m `seq` n `seq` xs `seq` False = undefined
        | BS.null xs = (m*10000000 + fromIntegral n, BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    loop8 m (n*10 + fromIntegral(w-0x30)) (BSU.unsafeTail xs)
              | otherwise -> (m*10000000 + fromIntegral n, xs)
    loop8 m n xs
        | m `seq` n `seq` xs `seq` False = undefined
        | BS.null xs = (m*100000000 + fromIntegral n, BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    loop0 (m*1000000000 + fromIntegral (n*10 + fromIntegral(w-0x30))) (BSU.unsafeTail xs)
              | otherwise -> (m*100000000 + fromIntegral n, xs)

readDecimalUnrolled_Int :: ByteString -> Int
readDecimalUnrolled_Int = unwrap . readDecimalUnrolled

readDecimalUnrolled_Int64 :: ByteString -> Int64
readDecimalUnrolled_Int64 = unwrap . readDecimalUnrolled

readDecimalUnrolled_Integer :: ByteString -> Integer
readDecimalUnrolled_Integer = unwrap . readDecimalUnrolled


-- A cleaned up version of readDecimalUnrolled. N.B., this version
-- doesn't guarantee prompt collection if the input string is
-- exhausted; though presumably clients will check for nullity and
-- discard empty strings themselves...
readDecimalUnrolled' :: Integral a => ByteString -> Maybe (a, ByteString)
readDecimalUnrolled' = start
    where
    isDecimal :: Word8 -> Bool
    {-# INLINE isDecimal #-}
    isDecimal w = 0x39 >= w && w >= 0x30

    toDigit :: Integral a => Word8 -> a
    {-# INLINE toDigit #-}
    toDigit w = fromIntegral (w - 0x30)

    addDigit :: Int -> Word8 -> Int
    {-# INLINE addDigit #-}
    addDigit n w = n * 10 + toDigit w

    start :: Integral a => ByteString -> Maybe (a, ByteString)
    start xs
        | BS.null xs = Nothing
        | otherwise  =
            case BSU.unsafeHead xs of
            w | isDecimal w -> Just $ loop0 (toDigit w) (BSU.unsafeTail xs)
              | otherwise   -> Nothing

    loop0 :: Integral a => a -> ByteString -> (a, ByteString)
    loop0 m xs
        | m `seq` xs `seq` False = undefined
        | not (BS.null xs) && isDecimal w =
            loop1 m (toDigit w) (BSU.unsafeTail xs)
        | otherwise = (m, xs)
        where w = BSU.unsafeHead xs

    loop1, loop2, loop3, loop4, loop5, loop6, loop7, loop8
        :: Integral a => a -> Int -> ByteString -> (a, ByteString)
    loop1 m n xs
        | m `seq` n `seq` xs `seq` False = undefined
        | not (BS.null xs) && isDecimal w =
            loop2 m (addDigit n w) (BSU.unsafeTail xs)
        | otherwise = (m*10 + fromIntegral n, xs)
        where w = BSU.unsafeHead xs
    loop2 m n xs
        | m `seq` n `seq` xs `seq` False = undefined
        | not (BS.null xs) && isDecimal w =
            loop3 m (addDigit n w) (BSU.unsafeTail xs)
        | otherwise = (m*100 + fromIntegral n, xs)
        where w = BSU.unsafeHead xs
    loop3 m n xs
        | m `seq` n `seq` xs `seq` False = undefined
        | not (BS.null xs) && isDecimal w =
            loop4 m (addDigit n w) (BSU.unsafeTail xs)
        | otherwise = (m*1000 + fromIntegral n, xs)
        where w = BSU.unsafeHead xs
    loop4 m n xs
        | m `seq` n `seq` xs `seq` False = undefined
        | not (BS.null xs) && isDecimal w =
            loop5 m (addDigit n w) (BSU.unsafeTail xs)
        | otherwise = (m*10000 + fromIntegral n, xs)
        where w = BSU.unsafeHead xs
    loop5 m n xs
        | m `seq` n `seq` xs `seq` False = undefined
        | not (BS.null xs) && isDecimal w =
            loop6 m (addDigit n w) (BSU.unsafeTail xs)
        | otherwise = (m*100000 + fromIntegral n, xs)
        where w = BSU.unsafeHead xs
    loop6 m n xs
        | m `seq` n `seq` xs `seq` False = undefined
        | not (BS.null xs) && isDecimal w =
            loop7 m (addDigit n w) (BSU.unsafeTail xs)
        | otherwise = (m*1000000 + fromIntegral n, xs)
        where w = BSU.unsafeHead xs
    loop7 m n xs
        | m `seq` n `seq` xs `seq` False = undefined
        | not (BS.null xs) && isDecimal w =
            loop8 m (addDigit n w) (BSU.unsafeTail xs)
        | otherwise = (m*10000000 + fromIntegral n, xs)
        where w = BSU.unsafeHead xs
    loop8 m n xs
        | m `seq` n `seq` xs `seq` False = undefined
        | not (BS.null xs) && isDecimal w =
            loop0 (m*1000000000 + fromIntegral (addDigit n w))
                  (BSU.unsafeTail xs)
        | otherwise = (m*100000000 + fromIntegral n, xs)
        where w = BSU.unsafeHead xs

readDecimalUnrolled'_Int :: ByteString -> Int
readDecimalUnrolled'_Int = unwrap . readDecimalUnrolled'

readDecimalUnrolled'_Int64 :: ByteString -> Int64
readDecimalUnrolled'_Int64 = unwrap . readDecimalUnrolled'

readDecimalUnrolled'_Integer :: ByteString -> Integer
readDecimalUnrolled'_Integer = unwrap . readDecimalUnrolled'

----------------------------------------------------------------
-- Try to add a fast track that removes the null tests. Doesn't
-- help; hurts a little.
readDecimalUnrolledAlt_Int64 :: ByteString -> Int64
readDecimalUnrolledAlt_Int64 = unwrap . start
    where
    start xs
        | BS.null xs = Nothing
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    Just $ loop0 (fromIntegral(w-0x30)) (BSU.unsafeTail xs)
              | otherwise -> Nothing

    loop0 :: Int64 -> ByteString -> (Int64, ByteString)
    loop0 m xs
        | m `seq` xs `seq` False = undefined
        | BS.length xs >= 9 =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    fast1 m (fromIntegral(w-0x30)) (BSU.unsafeTail xs)
              | otherwise -> (m, xs)
        | BS.null xs     = (m, BS.empty)
        | otherwise      =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    finish1 m (fromIntegral(w-0x30)) (BSU.unsafeTail xs)
              | otherwise -> (m, xs)

    fast1, fast2, fast3, fast4, fast5, fast6, fast7, fast8 :: Int64 -> Int -> ByteString -> (Int64, ByteString)
    fast1 m n xs
        | m `seq` n `seq` xs `seq` False = undefined
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    fast2 m (n*10 + fromIntegral(w-0x30)) (BSU.unsafeTail xs)
              | otherwise -> (m*10 + fromIntegral n, xs)
    fast2 m n xs
        | m `seq` n `seq` xs `seq` False = undefined
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    fast3 m (n*10 + fromIntegral(w-0x30)) (BSU.unsafeTail xs)
              | otherwise -> (m*100 + fromIntegral n, xs)
    fast3 m n xs
        | m `seq` n `seq` xs `seq` False = undefined
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    fast4 m (n*10 + fromIntegral(w-0x30)) (BSU.unsafeTail xs)
              | otherwise -> (m*1000 + fromIntegral n, xs)
    fast4 m n xs
        | m `seq` n `seq` xs `seq` False = undefined
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    fast5 m (n*10 + fromIntegral(w-0x30)) (BSU.unsafeTail xs)
              | otherwise -> (m*10000 + fromIntegral n, xs)
    fast5 m n xs
        | m `seq` n `seq` xs `seq` False = undefined
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    fast6 m (n*10 + fromIntegral(w-0x30)) (BSU.unsafeTail xs)
              | otherwise -> (m*100000 + fromIntegral n, xs)
    fast6 m n xs
        | m `seq` n `seq` xs `seq` False = undefined
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    fast7 m (n*10 + fromIntegral(w-0x30)) (BSU.unsafeTail xs)
              | otherwise -> (m*1000000 + fromIntegral n, xs)
    fast7 m n xs
        | m `seq` n `seq` xs `seq` False = undefined
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    fast8 m (n*10 + fromIntegral(w-0x30)) (BSU.unsafeTail xs)
              | otherwise -> (m*10000000 + fromIntegral n, xs)
    fast8 m n xs
        | m `seq` n `seq` xs `seq` False = undefined
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    loop0 (m*1000000000 + fromIntegral (n*10 + fromIntegral(w-0x30))) (BSU.unsafeTail xs)
              | otherwise -> (m*100000000 + fromIntegral n, xs)

    finish1, finish2, finish3, finish4, finish5, finish6, finish7, finish8 :: Int64 -> Int -> ByteString -> (Int64, ByteString)
    finish1 m n xs
        | m `seq` n `seq` xs `seq` False = undefined
        | BS.null xs = (m*10 + fromIntegral n, BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    finish2 m (n*10 + fromIntegral(w-0x30)) (BSU.unsafeTail xs)
              | otherwise -> (m*10 + fromIntegral n, xs)
    finish2 m n xs
        | m `seq` n `seq` xs `seq` False = undefined
        | BS.null xs = (m*100 + fromIntegral n, BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    finish3 m (n*10 + fromIntegral(w-0x30)) (BSU.unsafeTail xs)
              | otherwise -> (m*100 + fromIntegral n, xs)
    finish3 m n xs
        | m `seq` n `seq` xs `seq` False = undefined
        | BS.null xs = (m*1000 + fromIntegral n, BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    finish4 m (n*10 + fromIntegral(w-0x30)) (BSU.unsafeTail xs)
              | otherwise -> (m*1000 + fromIntegral n, xs)
    finish4 m n xs
        | m `seq` n `seq` xs `seq` False = undefined
        | BS.null xs = (m*10000 + fromIntegral n, BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    finish5 m (n*10 + fromIntegral(w-0x30)) (BSU.unsafeTail xs)
              | otherwise -> (m*10000 + fromIntegral n, xs)
    finish5 m n xs
        | m `seq` n `seq` xs `seq` False = undefined
        | BS.null xs = (m*100000 + fromIntegral n, BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    finish6 m (n*10 + fromIntegral(w-0x30)) (BSU.unsafeTail xs)
              | otherwise -> (m*100000 + fromIntegral n, xs)
    finish6 m n xs
        | m `seq` n `seq` xs `seq` False = undefined
        | BS.null xs = (m*1000000 + fromIntegral n, BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    finish7 m (n*10 + fromIntegral(w-0x30)) (BSU.unsafeTail xs)
              | otherwise -> (m*1000000 + fromIntegral n, xs)
    finish7 m n xs
        | m `seq` n `seq` xs `seq` False = undefined
        | BS.null xs = (m*10000000 + fromIntegral n, BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    finish8 m (n*10 + fromIntegral(w-0x30)) (BSU.unsafeTail xs)
              | otherwise -> (m*10000000 + fromIntegral n, xs)
    finish8 m n xs
        | m `seq` n `seq` xs `seq` False = undefined
        | BS.null xs = (m*100000000 + fromIntegral n, BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    ( m*1000000000 + fromIntegral (n*10 + fromIntegral(w-0x30))
                    , BSU.unsafeTail xs)
              | otherwise -> (m*100000000 + fromIntegral n, xs)


----------------------------------------------------------------
-- Do a three stage unrolling for Integer. Only gives a marginal
-- improvement, though really the payoff would be for things /much/
-- larger than 64-bits.
readDecimalTwiceUnrolled_Integer :: ByteString -> Integer
readDecimalTwiceUnrolled_Integer = unwrap . start
    where
    start xs
        | BS.null xs = Nothing
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    Just $ go00 (fromIntegral(w-0x30)) (BSU.unsafeTail xs)
              | otherwise -> Nothing

    go00 :: Integer -> ByteString -> (Integer, ByteString)
    go00 o xs
        | o `seq` xs `seq` False = undefined
        | BS.null xs = (o, BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    go01 o (fromIntegral(w-0x30)) (BSU.unsafeTail xs)
              | otherwise -> (o, xs)

    go01, go02, go03, go04, go05, go06, go07, go08
        :: Integer -> Int -> ByteString -> (Integer, ByteString)
    go01 o n xs
        | o `seq` n `seq` xs `seq` False = undefined
        | BS.null xs = (o*10 + fromIntegral n, BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    go02 o (n*10 + fromIntegral(w-0x30)) (BSU.unsafeTail xs)
              | otherwise -> (o*10 + fromIntegral n, xs)
    go02 o n xs
        | o `seq` n `seq` xs `seq` False = undefined
        | BS.null xs = (o*100 + fromIntegral n, BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    go03 o (n*10 + fromIntegral(w-0x30)) (BSU.unsafeTail xs)
              | otherwise -> (o*100 + fromIntegral n, xs)
    go03 o n xs
        | o `seq` n `seq` xs `seq` False = undefined
        | BS.null xs = (o*1000 + fromIntegral n, BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    go04 o (n*10 + fromIntegral(w-0x30)) (BSU.unsafeTail xs)
              | otherwise -> (o*1000 + fromIntegral n, xs)
    go04 o n xs
        | o `seq` n `seq` xs `seq` False = undefined
        | BS.null xs = (o*10000 + fromIntegral n, BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    go05 o (n*10 + fromIntegral(w-0x30)) (BSU.unsafeTail xs)
              | otherwise -> (o*10000 + fromIntegral n, xs)
    go05 o n xs
        | o `seq` n `seq` xs `seq` False = undefined
        | BS.null xs = (o*100000 + fromIntegral n, BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    go06 o (n*10 + fromIntegral(w-0x30)) (BSU.unsafeTail xs)
              | otherwise -> (o*100000 + fromIntegral n, xs)
    go06 o n xs
        | o `seq` n `seq` xs `seq` False = undefined
        | BS.null xs = (o*1000000 + fromIntegral n, BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    go07 o (n*10 + fromIntegral(w-0x30)) (BSU.unsafeTail xs)
              | otherwise -> (o*1000000 + fromIntegral n, xs)
    go07 o n xs
        | o `seq` n `seq` xs `seq` False = undefined
        | BS.null xs = (o*10000000 + fromIntegral n, BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    go08 o (n*10 + fromIntegral(w-0x30)) (BSU.unsafeTail xs)
              | otherwise -> (o*10000000 + fromIntegral n, xs)
    go08 o n xs
        | o `seq` n `seq` xs `seq` False = undefined
        | BS.null xs = (o*100000000 + fromIntegral n, BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    go90 o (fromIntegral (n*10 + fromIntegral(w-0x30))) (BSU.unsafeTail xs)
              | otherwise -> (o*100000000 + fromIntegral n, xs)

    go90 :: Integer -> Int64 -> ByteString -> (Integer, ByteString)
    go90 o m xs
        | o `seq` m `seq` xs `seq` False = undefined
        | BS.null xs = (o*1000000000 + fromIntegral m, BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    go91 o m (fromIntegral(w-0x30)) (BSU.unsafeTail xs)
              | otherwise -> (o*1000000000 + fromIntegral m, xs)

    go91, go92, go93, go94, go95, go96, go97, go98
        :: Integer -> Int64 -> Int -> ByteString -> (Integer, ByteString)
    go91 o m n xs
        | o `seq` m `seq` n `seq` xs `seq` False = undefined
        | BS.null xs = (o*1000000000000 + fromIntegral (m*10), BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    go92 o m (n*10 + fromIntegral(w-0x30)) (BSU.unsafeTail xs)
              | otherwise -> (o*1000000000000 + fromIntegral (m*10), xs)
    go92 o m n xs
        | o `seq` m `seq` n `seq` xs `seq` False = undefined
        | BS.null xs = (o*10000000000000 + fromIntegral (m*100), BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    go93 o m (n*10 + fromIntegral(w-0x30)) (BSU.unsafeTail xs)
              | otherwise -> (o*10000000000000 + fromIntegral (m*100), xs)
    go93 o m n xs
        | o `seq` m `seq` n `seq` xs `seq` False = undefined
        | BS.null xs = (o*100000000000000 + fromIntegral (m*1000), BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    go94 o m (n*10 + fromIntegral(w-0x30)) (BSU.unsafeTail xs)
              | otherwise -> (o*100000000000000 + fromIntegral (m*1000), xs)
    go94 o m n xs
        | o `seq` m `seq` n `seq` xs `seq` False = undefined
        | BS.null xs = (o*1000000000000000 + fromIntegral (m*10000), BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    go95 o m (n*10 + fromIntegral(w-0x30)) (BSU.unsafeTail xs)
              | otherwise -> (o*1000000000000000 + fromIntegral (m*10000), xs)
    go95 o m n xs
        | o `seq` m `seq` n `seq` xs `seq` False = undefined
        | BS.null xs = ( o*10000000000000000 + fromIntegral (m*100000)
                       , BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    go96 o m (n*10 + fromIntegral(w-0x30)) (BSU.unsafeTail xs)
              | otherwise ->
                        (o*10000000000000000 + fromIntegral
                        (m*100000), xs)
    go96 o m n xs
        | o `seq` m `seq` n `seq` xs `seq` False = undefined
        | BS.null xs = ( o*100000000000000000 + fromIntegral (m*1000000)
                       , BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    go97 o m (n*10 + fromIntegral(w-0x30)) (BSU.unsafeTail xs)
              | otherwise ->
                        (o*100000000000000000 + fromIntegral
                        (m*1000000), xs)
    go97 o m n xs
        | o `seq` m `seq` n `seq` xs `seq` False = undefined
        | BS.null xs = ( o*1000000000000000000 + fromIntegral (m*10000000)
                       , BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    go98 o m (n*10 + fromIntegral(w-0x30)) (BSU.unsafeTail xs)
              | otherwise ->
                        (o*1000000000000000000 + fromIntegral
                        (m*10000000), xs)
    go98 o m n xs
        | o `seq` m `seq` n `seq` xs `seq` False = undefined
        | BS.null xs = ( o*10000000000000000000 + fromIntegral (m*100000000)
                       , BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    go00
                        (o*10000000000000000000 + fromIntegral
                        (m*1000000000 + fromIntegral
                        (n*10 + fromIntegral(w-0x30))))
                        (BSU.unsafeTail xs)
              | otherwise ->
                        (o*10000000000000000000 + fromIntegral
                        (m*100000000), xs)


----------------------------------------------------------------
-- The requested version without @Maybe(_,ByteString)@ wrapping.

readDecimalUnwrapped_Int      :: ByteString -> Int
readDecimalUnwrapped_Int      = readDecimalUnwrapped

readDecimalUnwrapped_Int64    :: ByteString -> Int64
readDecimalUnwrapped_Int64    = readDecimalUnwrapped

readDecimalUnwrapped_Integer  :: ByteString -> Integer
readDecimalUnwrapped_Integer  = readDecimalUnwrapped

readDecimalUnwrapped :: Integral a => ByteString -> a
readDecimalUnwrapped = start
    where
    isDecimal :: Word8 -> Bool
    {-# INLINE isDecimal #-}
    isDecimal w = 0x39 >= w && w >= 0x30

    toDigit :: Integral a => Word8 -> a
    {-# INLINE toDigit #-}
    toDigit w = fromIntegral (w - 0x30)

    addDigit :: Int -> Word8 -> Int
    {-# INLINE addDigit #-}
    addDigit n w = n * 10 + toDigit w

    start xs
        | BS.null xs = 0
        | otherwise  =
            case BSU.unsafeHead xs of
            w | isDecimal w -> loop0 (toDigit w) (BSU.unsafeTail xs)
              | otherwise   -> 0

    loop0 :: Integral a => a -> ByteString -> a
    loop0 m xs
        | m `seq` xs `seq` False = undefined
        | BS.null xs = m
        | otherwise  =
            case BSU.unsafeHead xs of
            w | isDecimal w -> loop1 m (toDigit w) (BSU.unsafeTail xs)
              | otherwise   -> m

    loop1, loop2, loop3, loop4, loop5, loop6, loop7, loop8
        :: Integral a => a -> Int -> ByteString -> a
    loop1 m n xs
        | m `seq` n `seq` xs `seq` False = undefined
        | BS.null xs = m*10 + fromIntegral n
        | otherwise  =
            case BSU.unsafeHead xs of
            w | isDecimal w -> loop2 m (addDigit n w) (BSU.unsafeTail xs)
              | otherwise   -> m*10 + fromIntegral n
    loop2 m n xs
        | m `seq` n `seq` xs `seq` False = undefined
        | BS.null xs = m*100 + fromIntegral n
        | otherwise  =
            case BSU.unsafeHead xs of
            w | isDecimal w -> loop3 m (addDigit n w) (BSU.unsafeTail xs)
              | otherwise   -> m*100 + fromIntegral n
    loop3 m n xs
        | m `seq` n `seq` xs `seq` False = undefined
        | BS.null xs = m*1000 + fromIntegral n
        | otherwise  =
            case BSU.unsafeHead xs of
            w | isDecimal w -> loop4 m (addDigit n w) (BSU.unsafeTail xs)
              | otherwise   -> m*1000 + fromIntegral n
    loop4 m n xs
        | m `seq` n `seq` xs `seq` False = undefined
        | BS.null xs = m*10000 + fromIntegral n
        | otherwise  =
            case BSU.unsafeHead xs of
            w | isDecimal w -> loop5 m (addDigit n w) (BSU.unsafeTail xs)
              | otherwise   -> m*10000 + fromIntegral n
    loop5 m n xs
        | m `seq` n `seq` xs `seq` False = undefined
        | BS.null xs = m*100000 + fromIntegral n
        | otherwise  =
            case BSU.unsafeHead xs of
            w | isDecimal w -> loop6 m (addDigit n w) (BSU.unsafeTail xs)
              | otherwise   -> m*100000 + fromIntegral n
    loop6 m n xs
        | m `seq` n `seq` xs `seq` False = undefined
        | BS.null xs = m*1000000 + fromIntegral n
        | otherwise  =
            case BSU.unsafeHead xs of
            w | isDecimal w -> loop7 m (addDigit n w) (BSU.unsafeTail xs)
              | otherwise   -> m*1000000 + fromIntegral n
    loop7 m n xs
        | m `seq` n `seq` xs `seq` False = undefined
        | BS.null xs = m*10000000 + fromIntegral n
        | otherwise  =
            case BSU.unsafeHead xs of
            w | isDecimal w -> loop8 m (addDigit n w) (BSU.unsafeTail xs)
              | otherwise   -> m*10000000 + fromIntegral n
    loop8 m n xs
        | m `seq` n `seq` xs `seq` False = undefined
        | BS.null xs = m*100000000 + fromIntegral n
        | otherwise  =
            case BSU.unsafeHead xs of
            w | isDecimal w -> loop0
                    (m*1000000000 + fromIntegral (addDigit n w))
                    (BSU.unsafeTail xs)
              | otherwise   -> m*100000000 + fromIntegral n

----------------------------------------------------------------
----------------------------------------------------------------
-- The versions currently used by the library
readDecimal_Int      :: ByteString -> Int
readDecimal_Int      = unwrap . BSLex.readDecimal

readDecimal_Int64    :: ByteString -> Int64
readDecimal_Int64    = unwrap . BSLex.readDecimal

readDecimal_Integer  :: ByteString -> Integer
readDecimal_Integer  = unwrap . BSLex.readDecimal


readDecimal__Int      :: ByteString -> Int
readDecimal__Int      = BSLex.readDecimal_

readDecimal__Int64    :: ByteString -> Int64
readDecimal__Int64    = BSLex.readDecimal_

readDecimal__Integer  :: ByteString -> Integer
readDecimal__Integer  = BSLex.readDecimal_

----------------------------------------------------------------
----------------------------------------------------------------
-- A QuickCheck property. Test that for a number >= 0, converting it to
-- a string using show and then reading the value back with the function
-- under test returns the original value.
-- The functions under test only work on Natural numbers (the Conent-Length
-- field in a HTTP header is always >= 0) so we check the absolute value of
-- the value that QuickCheck generates for us.
prop_read_show_idempotent
    :: (Num a, Eq a, Show a) => (ByteString -> a) -> a -> Bool
prop_read_show_idempotent freader x =
    let px = abs x
    in px == freader (BS8.pack $ show px)


runQuickCheckTests :: IO ()
runQuickCheckTests = do
    putStrLn "Checking readIntegralTC (buggy)..."
    QC.quickCheck (prop_read_show_idempotent readIntTC)
    QC.quickCheck (prop_read_show_idempotent readInt64TC)
    QC.quickCheck (prop_read_show_idempotent readIntegerTC)
    --
    putStrLn "Checking readIntegralMH..."
    QC.quickCheck (prop_read_show_idempotent readIntMH)
    QC.quickCheck (prop_read_show_idempotent readInt64MH)
    QC.quickCheck (prop_read_show_idempotent readIntegerMH)
    --
    putStrLn "Checking readIntegralMHFast..."
    QC.quickCheck (prop_read_show_idempotent readIntMHFast)
    QC.quickCheck (prop_read_show_idempotent readInt64MHFast)
    QC.quickCheck (prop_read_show_idempotent readIntegerMHFast)
    --
    ----putStrLn "Checking readIntWarp..."
    ----QC.quickCheck (prop_read_show_idempotent readIntWarp)
    --
    putStrLn "Checking readDecimalOrig..."
    QC.quickCheck (prop_read_show_idempotent readDecimalOrig_Int)
    QC.quickCheck (prop_read_show_idempotent readDecimalOrig_Int64)
    QC.quickCheck (prop_read_show_idempotent readDecimalOrig_Integer)
    putStrLn "Checking readDecimalHack (buggy)..."
    QC.quickCheck (prop_read_show_idempotent readDecimalHack_Int64)
    QC.quickCheck (prop_read_show_idempotent readDecimalHack_Integer)
    putStrLn "Checking readDecimalUnrolled..."
    QC.quickCheck (prop_read_show_idempotent readDecimalUnrolled_Int)
    QC.quickCheck (prop_read_show_idempotent readDecimalUnrolled'_Int)
    QC.quickCheck (prop_read_show_idempotent readDecimalUnrolled_Int64)
    QC.quickCheck (prop_read_show_idempotent readDecimalUnrolled'_Int64)
    QC.quickCheck (prop_read_show_idempotent readDecimalUnrolled_Integer)
    QC.quickCheck (prop_read_show_idempotent readDecimalUnrolled'_Integer)
    putStrLn "Checking readDecimalUnrolledAlt_Int64..."
    QC.quickCheck (prop_read_show_idempotent readDecimalUnrolledAlt_Int64)
    putStrLn "Checking readDecimalTwiceUnrolled_Integer..."
    QC.quickCheck (prop_read_show_idempotent readDecimalTwiceUnrolled_Integer)
    putStrLn "Checking readDecimal (current)..."
    QC.quickCheck (prop_read_show_idempotent readDecimal_Int)
    QC.quickCheck (prop_read_show_idempotent readDecimal_Int64)
    QC.quickCheck (prop_read_show_idempotent readDecimal_Integer)
    putStrLn "Checking readDecimalUnwrapped..."
    QC.quickCheck (prop_read_show_idempotent readDecimalUnwrapped_Int)
    QC.quickCheck (prop_read_show_idempotent readDecimalUnwrapped_Int64)
    QC.quickCheck (prop_read_show_idempotent readDecimalUnwrapped_Integer)


runCriterionTests :: ByteString -> IO ()
runCriterionTests n =
    defaultMain
        [ bgroup "naive"
            [ bench "Numeric.readDec @Integer" $ nf numericReadDec_Integer n
            , bench "BS.Char8.foldl' @Int"     $ nf bs8foldl_Int n
            , bench "BS.foldl' @Integer"       $ nf bsfoldl_Integer n
            , bench "BS.Char8.readInt"         $ nf bs8readInt n
            , bench "BS.Char8.readInteger"     $ nf bs8readInteger n
            ]
        , bgroup "readIntegralTC (buggy)"
            [ bench "Int"           $ nf readIntTC n
            , bench "Int64"         $ nf readInt64TC n
            , bench "Integer"       $ nf readIntegerTC n
            ]
        , bgroup "readIntegralMH"
            [ bench "Int"           $ nf readIntMH n
            , bench "Int64"         $ nf readInt64MH n
            , bench "Integer"       $ nf readIntegerMH n
            ]
        , bgroup "readIntegralMHFast"
            [ bench "Int"           $ nf readIntMHFast      n
            , bench "Int64"         $ nf readInt64MHFast    n
            , bench "Integer"       $ nf readIntegerMHFast  n
            ]
        ---- , bench "readIntWarp"   $ nf readIntWarp n
        , bgroup "readDecimalOrig"
            [ bench "Int"           $ nf readDecimalOrig_Int     n
            , bench "Int64"         $ nf readDecimalOrig_Int64   n
            , bench "Integer"       $ nf readDecimalOrig_Integer n
            ]
        , bgroup "readDecimalHack (buggy)"
            [ bench "Int64"         $ nf readDecimalHack_Int64 n
            , bench "Integer"       $ nf readDecimalHack_Integer n
            ]
        , bgroup "readDecimalUnrolled"
            [ bench "Int"          $ nf readDecimalUnrolled_Int          n
            , bench "Int'"         $ nf readDecimalUnrolled'_Int         n
            , bench "Int64"        $ nf readDecimalUnrolled_Int64        n
            , bench "Int64'"       $ nf readDecimalUnrolled'_Int64       n
            , bench "Int64_alt"    $ nf readDecimalUnrolledAlt_Int64     n
            , bench "Integer"      $ nf readDecimalUnrolled_Integer      n
            , bench "Integer'"     $ nf readDecimalUnrolled'_Integer     n
            , bench "Integer_alt"  $ nf readDecimalTwiceUnrolled_Integer n
            ]
        , bgroup "readDecimalUnwrapped"
            [ bench "Int"          $ nf readDecimalUnwrapped_Int     n
            , bench "Int64"        $ nf readDecimalUnwrapped_Int64   n
            , bench "Integer"      $ nf readDecimalUnwrapped_Integer n
            ]
        , bgroup "readDecimal (current)"
            [ bench "Int"          $ nf readDecimal_Int n
            , bench "Int64"        $ nf readDecimal_Int64 n
            , bench "Integer"      $ nf readDecimal_Integer n
            ]
        , bgroup "readDecimal_ (current)"
            [ bench "Int"          $ nf readDecimal__Int     n
            , bench "Int64"        $ nf readDecimal__Int64   n
            , bench "Integer"      $ nf readDecimal__Integer n
            ]
        ]

main :: IO ()
main = do
    putStrLn "Quickcheck tests."
    runQuickCheckTests
    putStrLn "Criterion tests."
    runCriterionTests "1234567898765432178979128361238162386182"
