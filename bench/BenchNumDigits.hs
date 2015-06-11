{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -XMagicHash #-}
----------------------------------------------------------------
--                                                    2011.05.30
-- |
-- Module      :  BenchNumDigits
-- Copyright   :  Copyright (c) 2011--2015 wren gayle romano
-- License     :  BSD2
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- A benchmark for comparing different definitions of functions for
-- getting the number of (decimal) digits in a number. In a previous
-- run the results were:
-- 
-- * numDigits     : 61.31797 ms +/- 275.4235 us
-- * numDigits'    : 56.50285 ms +/- 431.3593 us
-- * numDigitsTCO  : 67.88873 ms +/- 146.9378 us
-- * numDigitsTCO' : 70.09413 ms +/- 147.0889 us
--
-- So it looks like TCO is a lose, strict or not. But in the most
-- recent run:
--
-- * numDigits      : 58.79698 ms +/- 811.4708 us
-- * numDigits'     : 55.80930 ms +/- 358.7905 us
-- * numDigits''    : 42.49428 ms +/- 465.3396 us
-- * numDigitsI     : 41.63817 ms +/- 333.9922 us
-- * numDigitsI'    : 59.92484 ms +/- 1.143297 ms -- Yuck, what happened?
-- * numDigitsST    : 91.95964 ms +/- 271.4893 us
-- * numDigitsTCO   : 67.60948 ms +/- 413.0278 us
-- * numDigitsTCO'  : 55.78408 ms +/- 337.6619 us
--
-- Suggesting that strict TCO is a win... I think the only difference
-- was using SPECIALIZE in the former and INLINE in the latter (and
-- adding error messages in the latter). But then subsequent runs
-- replicate the earlier ones...
----------------------------------------------------------------
module BenchNumDigits (main) where

import Data.Bits
#if __GLASGOW_HASKELL__ >= 503
import GHC.Exts  ( Word(..), Int(..), shiftRL# )
#elif __GLASGOW_HASKELL__
import GlaExts   ( Word(..), Int(..), shiftRL# )
#else
import Data.Word (Word)
#endif

import Control.Monad.ST
import Data.STRef
 
import Criterion      (bench, nf)
import Criterion.Main (defaultMain)
----------------------------------------------------------------

main :: IO ()
main = defaultMain
    -- BUG: using an upper limit of 2^30 causes OOM failure!!
    [ bench "numDigits"      $ nf (seqMap $ numDigits      base) [0..limit]
    , bench "numDigits'"     $ nf (seqMap $ numDigits'     base) [0..limit]
    , bench "numDigits''"    $ nf (seqMap $ numDigits''    base) [0..limit]
    , bench "numDigitsI"     $ nf (seqMap $ numDigitsI     base) [0..limit]
    , bench "numDigitsI'"    $ nf (seqMap $ numDigitsI'    base) [0..limit]
    , bench "numDigitsST"    $ nf (seqMap $ numDigitsST    base) [0..limit]
    , bench "numDigitsTCO"   $ nf (seqMap $ numDigitsTCO   base) [0..limit]
    , bench "numDigitsTCO'"  $ nf (seqMap $ numDigitsTCO'  base) [0..limit]
    -- TODO: add the currently installed version
    ]
    where
    base  = 10
    limit = 2 ^ (20 :: Int)
    
    -- Not really map
    seqMap :: (Int -> Int) -> [Int] -> ()
    seqMap _ []     = ()
    seqMap f (x:xs) = f x `seq` seqMap f xs

----------------------------------------------------------------

_numDigits_nonpositiveBase :: String
{-# NOINLINE _numDigits_nonpositiveBase #-}
_numDigits_nonpositiveBase = "numDigits: base must be positive"

_numDigits_negativeNumber :: String
{-# NOINLINE _numDigits_negativeNumber #-}
_numDigits_negativeNumber  = "numDigits: number must be non-negative"

-- | The original version, from:
-- <http://www.haskell.org/pipermail/haskell-cafe/2009-August/065854.html>
numDigits :: (Integral b, Num a) => b -> b -> a
{-# INLINE numDigits #-}
-- numDigits b n | n < 0 = 1 + numDigits b (negate n)
numDigits b0 n0
    | b0 <= 0   = error _numDigits_nonpositiveBase
    | n0 <  0   = error _numDigits_negativeNumber
    | otherwise = 1 + fst (ilog b0 n0)
    where
    ilog b n
        | n < b     = (0, n)
        | otherwise =
            let (e, r) = ilog (b*b) n
            in  if r < b
                then (2*e, r)
                else (2*e+1, r `div` b)


-- | A slightly modified version of 'numDigits' to use 'quot' instead
-- of 'div'. (Also using more guard notation, but that's compiled
-- away.)
numDigits' :: (Integral b, Num a) => b -> b -> a
{-# INLINE numDigits' #-}
numDigits' b0 n0
    | b0 <= 0   = error _numDigits_nonpositiveBase
    | n0 <  0   = error _numDigits_negativeNumber
    | otherwise = 1 + fst (ilog' b0 n0)
    where
    ilog' b n
        | n < b     = (0, n)
        | r < b     = (2*e, r)
        | otherwise = (2*e+1, r `quot` b)
        where
        (e, r) = ilog' (b*b) n
-- TODO: is there any way to convince GHC that if b0>0 then b can
-- never be 0? If we could, that'd save a case match in the 'quot'.
-- Of course, we'd have to beware of overflow in order for it to
-- be true. @divInt#@ and @modInt#@ are defined in GHC.Base, but
-- @remInt#@ and @quotInt#@ appear to be primitive (GHC.Prim defines
-- them as _|_). So it looks like we can't get rid of the extra
-- case match; also, the overflow case match should be floated up
-- to just before the recursive call, instead of only in the @quot@.

-- | Slightly modified again, for ridiculous strictness.
numDigits'' :: (Integral b, Num a) => b -> b -> a
{-# INLINE numDigits'' #-}
numDigits'' b0 n0
    | b0 <= 0   = error _numDigits_nonpositiveBase
    | n0 <  0   = error _numDigits_negativeNumber
    | otherwise = 1 + fst (ilog'' b0 n0)
    where
    ilog'' b n
        | n < b     = (0, n)
        | r < b     = ((,) $! 2*e) r
        | otherwise = ((,) $! 2*e+1) $! (r `quot` b)
        where
        (e, r) = ilog'' (b*b) n


-- | Monomorphic, just to see where the limit might be.
numDigitsI :: Int -> Int -> Int
{-# INLINE numDigitsI #-}
numDigitsI b0 n0
    | b0 <= 0   = error _numDigits_nonpositiveBase
    | n0 <  0   = error _numDigits_negativeNumber
    | otherwise = 1 + fst (ilogI b0 n0)
    where
    ilogI b n
        | n < b     = (0, n)
        | r < b     = ((,) $! shiftR e 1) r
        | otherwise = ((,) $! (shiftR e 1 .|. 1)) $! (r `quot` b)
        where
        (e, r) = ilogI (b*b) n

-- | With the special GHC bit-twiddling hack
numDigitsI' :: Int -> Int -> Int
{-# INLINE numDigitsI' #-}
numDigitsI' b0 n0
    | b0 <= 0   = error _numDigits_nonpositiveBase
    | n0 <  0   = error _numDigits_negativeNumber
    | otherwise = fromIntegral (1 + fst (ilogI' b0 n0))
    where
    ilogI' b n
        | n < b     = (0, n)
        | r < b     = ((,) $! shiftRL e 1) r
        | otherwise = ((,) $! (shiftRL e 1 .|. 1)) $! (r `quot` b)
        where
        (e, r) = ilogI' (b*b) n

shiftRL :: Word -> Int -> Word
{-# INLINE shiftRL #-}
#if __GLASGOW_HASKELL__
-- GHC: use unboxing to get @shiftRL@ inlined.
shiftRL (W# w) (I# i) = W# (shiftRL# w i)
#else
shiftRL w i = shiftR w i
#endif


-- | Trying out mutation baby!
numDigitsST :: (Integral b, Num a) => b -> b -> a
{-# INLINE numDigitsST #-}
numDigitsST b0 n0
    | b0 <= 0   = error _numDigits_nonpositiveBase
    | n0 <  0   = error _numDigits_negativeNumber
    | otherwise = runST $ do
        e <- newSTRef 0
        r <- newSTRef 0
        ilogST e r b0 n0
        e' <- readSTRef e
        return $! e'+1
    where
    ilogST e r b n
        | n < b     = writeSTRef e 0 >> writeSTRef r n
        | otherwise = do
            ilogST e r (b*b) n
            e' <- readSTRef e
            r' <- readSTRef r
            if r' < b
                then do
                    writeSTRef e $! 2*e'
                else do
                    writeSTRef e $! 2*e'+1
                    writeSTRef r $! quot r' b


-- | Is it worth it to make it tail recursive? Time for Criterion!
numDigitsTCO :: (Integral b, Num a) => b -> b -> a
{-# INLINE numDigitsTCO #-}
numDigitsTCO b0 n0
    | b0 <= 0   = error _numDigits_nonpositiveBase
    | n0 <  0   = error _numDigits_negativeNumber
    | otherwise = 1 + ilogk b0 n0 const
    where
    ilogk b n k =
        if n < b
        then k 0 n
        else ilogk (b*b) n $ \e r ->
            if r < b
            then k (2*e) r
            else k (2*e+1) (r `quot` b)


-- | A version of 'numDigitsTCO' made strict in the accumulators.
-- Part of the problem seems to be that the continuation won't unbox
-- its arguments, no matter how hard I try.
numDigitsTCO' :: (Integral b, Num a) => b -> b -> a
{-# INLINE numDigitsTCO' #-}
numDigitsTCO' b0 n0
    | b0 <= 0   = error _numDigits_nonpositiveBase
    | n0 <  0   = error _numDigits_negativeNumber
    | otherwise = 1 + ilogk' b0 n0 const
    where
    ilogk' b n k =
        if n < b
        then k 0 n
        else ilogk' (b*b) n $ \e r ->
            if r < b
            then (k $! 2*e) r
            else (k $! 2*e+1) $! (r `quot` b)

-- Also Cf.,
-- <http://www.haskell.org/pipermail/haskell-cafe/2009-August/065856.html>
-- Henning Thielemann:
-- I thought a little about it. If I had to implement that in GMP it could
-- be done quite fast in many cases: Count the number of bits, say it is
-- 'k' and multiply with logBase 10 2. If 2^k and 2^(k+1)-1 have the same
-- number of decimal digits, we are done. Otherwise we have to process some
-- of the most significant bits. If the number is between n*2^k and
-- (n+1)*2^k-1 and both bounds have the same number of decimal digits
-- (logBase 10 n + k * logBase 10 2), we are also done. Only for numbers
-- close to powers of 10 we have to process the whole integer.

----------------------------------------------------------------
----------------------------------------------------------- fin.