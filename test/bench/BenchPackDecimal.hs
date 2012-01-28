{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
----------------------------------------------------------------
--                                                    2011.05.30
-- |
-- Module      :  BenchPackDecimal
-- Copyright   :  Copyright (c) 2011--2012 wren ng thornton
-- License     :  BSD
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- A benchmark for comparing different definitions of functions for
-- rendering Integral numbers as decimal ASCII ByteStrings.
--
-- * packDecimal  : 364.6638 ms +/- 346.2217 us
-- * packDecimal' : 136.8190 ms +/- 216.4754 us -- with buggy, numDigits
-- * packDecimal' : 212.6065 ms +/- 282.3224 us -- with correct numDigits
----------------------------------------------------------------
module BenchPackDecimal (main) where
 
import Criterion      (bench, nf)
import Criterion.Main (defaultMain)

import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Internal as BSI
import           Data.Word                (Word8)
import           Foreign.Ptr              (Ptr, plusPtr)
import           Foreign.Storable         (poke)
----------------------------------------------------------------

main :: IO ()
main = defaultMain
    -- BUG: using an upper limit of 2^30 causes OOM failure!!
    [ bench "packDecimal"   $ nf (seqMap packDecimal)   [0..limit]
    , bench "packDecimal'"  $ nf (seqMap packDecimal')  [0..limit]
    ]
    where
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
-- | Convert a positive integer into an (unsigned) ASCII decimal
-- string. Returns @Nothing@ on negative inputs.
packDecimal :: (Integral a) => a -> Maybe ByteString
{-# SPECIALIZE packDecimal ::
    Int     -> Maybe ByteString,
    Integer -> Maybe ByteString #-}
packDecimal = start
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
    | b0 <= 0   = error _numDigits_nonpositiveBase
    | n0 <  0   = error _numDigits_negativeNumber
    | otherwise = 1 + fst (ilog b0 n0)
    where
    -- See ./test/bench/BenchNumDigits.hs for implementation choices.
    ilog b n
        | n < b     = (0, n)
        | r < b     = ((,) $! 2*e) r
        | otherwise = ((,) $! 2*e+1) $! (r `quot` b)
        where
        (e, r) = ilog (b*b) n

_numDigits_nonpositiveBase :: String
{-# NOINLINE _numDigits_nonpositiveBase #-}
_numDigits_nonpositiveBase = "numDigits: base must be positive"

_numDigits_negativeNumber  :: String
{-# NOINLINE _numDigits_negativeNumber #-}
_numDigits_negativeNumber  = "numDigits: number must be non-negative"


packDecimal' :: (Integral a) => a -> Maybe ByteString
{-# SPECIALIZE packDecimal' ::
    Int     -> Maybe ByteString,
    Integer -> Maybe ByteString #-}
packDecimal' n0
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
----------------------------------------------------------- fin.
