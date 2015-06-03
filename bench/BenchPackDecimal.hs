{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
----------------------------------------------------------------
--                                                    2013.03.21
-- |
-- Module      :  BenchPackDecimal
-- Copyright   :  Copyright (c) 2011--2015 wren gayle romano
-- License     :  BSD2
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- A benchmark for comparing different definitions of functions for
-- rendering Integral numbers as decimal ASCII ByteStrings.
--
-- * packDecimal0 : 357.3809 ms +/- 1.883101 ms
-- * packDecimal1 : 210.0675 ms +/- 626.1323 us
-- * packDecimal2 : 176.6966 ms +/- 200.8797 us
-- * packDecimal3 : 153.9941 ms +/- 246.0675 us
----------------------------------------------------------------
module BenchPackDecimal (main) where

import Criterion      (bench, nf)
import Criterion.Main (defaultMain)

import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Char8    as BS8 (pack)
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Unsafe   as BSU
import           Data.Word                (Word8, Word64)
import           Foreign.Ptr              (Ptr, plusPtr)
import           Foreign.Storable         (poke)

import Data.ByteString.Lex.Integral (packDecimal)
----------------------------------------------------------------

main :: IO ()
main = defaultMain
    [ bench "packDecimal0 (naive)"  $ nf (seqMap packDecimal0) [0..limit]
    , bench "packDecimal1 (v0.4.0)" $ nf (seqMap packDecimal1) [0..limit]
    , bench "packDecimal2"          $ nf (seqMap packDecimal2) [0..limit]
    , bench "packDecimal3 (v0.4.2)" $ nf (seqMap packDecimal3) [0..limit]
    --
    , bench "packDecimal (current)" $ nf (seqMap packDecimal)  [0..limit]
    ]
    where
    -- BUG: using an upper limit of 2^30 causes OOM failure!!
    limit = 2 ^ (20 :: Int)
    
    -- Not really map
    seqMap :: (Int -> Maybe ByteString) -> [Int] -> ()
    seqMap _ []     = ()
    seqMap f (x:xs) =
        case f x of
        Nothing -> error "Broken implementation"
        Just y  -> y `seq` seqMap f xs

----------------------------------------------------------------

-- N.B., this one is correct, even for small unsigned types like Word8!
-- | Naively convert a positive integer into an (unsigned) ASCII
-- decimal string. Returns @Nothing@ on negative inputs.
packDecimal0 :: (Integral a) => a -> Maybe ByteString
{-# SPECIALIZE packDecimal0 ::
    Int     -> Maybe ByteString,
    Integer -> Maybe ByteString #-}
packDecimal0 = start
    where
    -- TODO: use a builder to preallocate a single buffer and then
    -- just fill it, instead of 'BS.cons'ing.
    start n
        | n < 0     = Nothing
        | otherwise = Just $ loop n BS.empty
    
    loop n xs
        | n `seq` xs `seq` False = undefined -- for strictness analysis
        | n <= 9    = BS.cons (fromIntegral n + 0x30) xs
        | otherwise =
            -- quotRem == divMod when both @n@ and @b@ are positive,
            -- and 'quotRem' is generally faster.
            let (q,r) = n `quotRem` 10
            in loop q (BS.cons (fromIntegral r + 0x30) xs)


-- | From 
-- <http://www.haskell.org/pipermail/haskell-cafe/2009-August/065854.html>
-- modified to use 'quot' instead of 'div', to ensure strictness,
-- and using more guard notation (but this last one's compiled away).
--
-- We must be careful about the input types here. When using small
-- unsigned types or very large values, the @b*b@ can overflow
-- causing @ilog@ to infinite loop. (E.g., the fourth squaring of
-- 10 overflows 32-bits (==1874919424 which is greater than the
-- third squaring. For 64-bit, the 5th squaring overflows, but it's
-- negative.) Forcing the type to Integer ensures correct behavior,
-- but makes it substantially slower.
numDigits :: Integer -> Integer -> Int
{-# INLINE numDigits #-}
numDigits b0 n0
    | b0 <= 0   = error (_numDigits ++ _nonpositiveBase)
    | n0 <  0   = error (_numDigits ++ _negativeNumber)
    | otherwise = 1 + fst (ilog b0 n0)
    where
    -- See ./test/bench/BenchNumDigits.hs for implementation choices.
    ilog b n
        | n < b     = (0, n)
        | r < b     = ((,) $! 2*e) r
        | otherwise = ((,) $! 2*e+1) $! (r `quot` b)
        where
        (e, r) = ilog (b*b) n


packDecimal1 :: (Integral a) => a -> Maybe ByteString
{-# SPECIALIZE packDecimal1 ::
    Int     -> Maybe ByteString,
    Integer -> Maybe ByteString #-}
packDecimal1 n0
    | n0 < 0    = Nothing
    | otherwise = Just $
        let size = numDigits 10 (toInteger n0)
        in  BSI.unsafeCreate size (\p0 -> loop n0 (p0 `plusPtr` (size - 1)))
    where
    loop :: (Integral a) => a -> Ptr Word8 -> IO ()
    loop n p
        | n <= 9    = do
            poke p (0x30 + fromIntegral n)
        | otherwise = do
            let (q,r) = n `quotRem` 10
            poke p (0x30 + fromIntegral r)
            loop q (p `plusPtr` negate 1)

----------------------------------------------------------------
-- This implementation is from:
-- <http://www.serpentine.com/blog/2013/03/20/whats-good-for-c-is-good-for-haskell/>
--
-- | Compute the number of base-@10@ digits required to represent
-- a number @n@. N.B., this implementation is unsafe and will throw
-- errors if the number is negative.
numDecimalDigits :: (Integral a) => a -> Int
{-# INLINE numDecimalDigits #-}
numDecimalDigits n0
    | n0 < 0    = error (_numDecimalDigits ++ _negativeNumber)
    -- BUG: need to check n0 to be sure we won't overflow Word64
    | otherwise = go 1 (fromIntegral n0 :: Word64)
    where
    fin n bound = if n >= bound then 1 else 0
    go k n
        | k `seq` False = undefined -- For strictness analysis
        | n < 10        = k
        | n < 100       = k + 1
        | n < 1000      = k + 2
        | n < 1000000000000 =
            k + if n < 100000000
                then if n < 1000000
                    then if n < 10000
                        then 3
                        else 4 + fin n 100000
                    else 6 + fin n 10000000
                else if n < 10000000000
                    then 8 + fin n 1000000000
                    else 10 + fin n 100000000000
        | otherwise = go (k + 12) (n `quot` 1000000000000)


-- | Same as 'packDecimal1', except using 'numDecimalDigits' instead of 'numDigits'.
packDecimal2 :: (Integral a) => a -> Maybe ByteString
{-# SPECIALIZE packDecimal2 ::
    Int     -> Maybe ByteString,
    Integer -> Maybe ByteString #-}
packDecimal2 n0
    | n0 < 0    = Nothing
    | otherwise = Just $
        let size = numDecimalDigits n0
        in  BSI.unsafeCreate size (\p0 -> loop n0 (p0 `plusPtr` (size - 1)))
    where
    loop :: (Integral a) => a -> Ptr Word8 -> IO ()
    loop n p
        | n <= 9    = do
            poke p (0x30 + fromIntegral n)
        | otherwise = do
            let (q,r) = n `quotRem` 10
            poke p (0x30 + fromIntegral r)
            loop q (p `plusPtr` negate 1)


-- | Revising 'packDecimal2' to write two digits at a time.
packDecimal3 :: (Integral a) => a -> Maybe ByteString
{-# SPECIALIZE packDecimal3 ::
    Int     -> Maybe ByteString,
    Integer -> Maybe ByteString #-}
packDecimal3 n0
    | n0 < 0    = Nothing
    | otherwise = Just $
        let size = numDecimalDigits n0
        in  BSI.unsafeCreate size (\p0 -> loop n0 (p0 `plusPtr` (size - 1)))
    where
    getDigit :: Int -> Word8
    getDigit = BSU.unsafeIndex packDecimal3_digits

    -- loop :: iota a. a -> Ptr Word8 -> IO ()
    loop n p
        | n >= 100  = do
            let (q,r) = n `quotRem` 100
            write2 r p
            loop q (p `plusPtr` negate 2)
        | n >= 10   = write2 n p
        | otherwise = poke p (0x30 + fromIntegral n)
    
    -- write2 :: iota a. a -> Ptr Word8 -> IO ()
    write2 i0 p = do
        let i = fromIntegral i0; j = i + i
        poke p                      (getDigit $! j + 1)
        poke (p `plusPtr` negate 1) (getDigit j)

packDecimal3_digits :: ByteString
{-# NOINLINE packDecimal3_digits #-}
packDecimal3_digits = BS8.pack
    "0001020304050607080910111213141516171819\
    \2021222324252627282930313233343536373839\
    \4041424344454647484950515253545556575859\
    \6061626364656667686970717273747576777879\
    \8081828384858687888990919293949596979899"

----------------------------------------------------------------
_numDigits :: String
_numDigits = "numDigits"
{-# NOINLINE _numDigits #-}

_numDecimalDigits :: String
_numDecimalDigits = "numDecimalDigits"
{-# NOINLINE _numDecimalDigits #-}

_nonpositiveBase :: String
_nonpositiveBase = ": base must be greater than one"
{-# NOINLINE _nonpositiveBase #-}

_negativeNumber :: String
_negativeNumber = ": number must be non-negative"
{-# NOINLINE _negativeNumber #-}

----------------------------------------------------------------
----------------------------------------------------------- fin.
