{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
----------------------------------------------------------------
--                                                    2012.01.26
-- |
-- Copyright   :  Copyright (c) 2010--2012 wren ng thornton
-- License     :  BSD3
-- Maintainer  :  wren@community.haskell.org
--
-- Correctness testing for "Data.ByteString.Lex.Integral".
----------------------------------------------------------------

import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Char8        as BS8
import           Data.ByteString.Lex.Integral
import           Data.Int
import qualified Test.QuickCheck              as QC
import qualified Test.SmallCheck              as SC

----------------------------------------------------------------
----- QuickCheck\/SmallCheck properties

-- | Converting a non-negative number to a string using 'show' and
-- then reading it back using 'readDecimal' returns the original
-- number.
prop_readShowIdempotent_unsigned :: (Show a, Integral a) => a -> Bool
prop_readShowIdempotent_unsigned x =
    let px = abs x
    in  Just (px, BS.empty) == (readDecimal . BS8.pack . show) px


-- | Converting a number to a string using 'show' and then reading
-- it back using @'readSigned' 'readDecimal'@ returns the original
-- number.
prop_readShowIdempotent_signed :: (Show a, Integral a) => a -> Bool
prop_readShowIdempotent_signed x =
    Just (x, BS.empty) == (readSigned readDecimal . BS8.pack . show) x


-- | Converting a non-negative number to a bytestring using 'packDecimal' and then reading it back using 'read' returns the original number.
prop_showReadIdempotent_unsigned :: (Read a, Integral a) => a -> Bool
prop_showReadIdempotent_unsigned x =
    let px = abs x
    in  px == (read . maybe "" BS8.unpack . packDecimal) px

----------------------------------------------------------------
{-
-- | A more obviously correct but much slower implementation than
-- the public one.
packDecimal :: (Integral a) => a -> Maybe ByteString
packDecimal = start
    where
    start n0
        | n0 < 0    = Nothing
        | otherwise = Just $ loop n0 BS.empty
    
    loop n xs
        | n `seq` xs `seq` False = undefined -- for strictness analysis
        | n <= 9    = BS.cons (0x30 + fromIntegral n) xs
        | otherwise =
            let (q,r) = n `quotRem` 10
            in loop q (BS.cons (0x30 + fromIntegral r) xs)
-}

----------------------------------------------------------------
main :: IO ()
main = do
    putStrLn "QuickCheck tests."
    putStrLn "Checking prop_readShowIdempotent_unsigned..."
    QC.quickCheck (prop_readShowIdempotent_unsigned :: Int -> Bool)
    QC.quickCheck (prop_readShowIdempotent_unsigned :: Int64 -> Bool)
    QC.quickCheck (prop_readShowIdempotent_unsigned :: Integer -> Bool)
    --
    putStrLn "Checking prop_readShowIdempotent_signed..."
    QC.quickCheck (prop_readShowIdempotent_signed :: Int -> Bool)
    QC.quickCheck (prop_readShowIdempotent_signed :: Int64 -> Bool)
    QC.quickCheck (prop_readShowIdempotent_signed :: Integer -> Bool)
    --
    putStrLn "Checking prop_showReadIdempotent_unsigned..."
    QC.quickCheck (prop_showReadIdempotent_unsigned :: Int -> Bool)
    QC.quickCheck (prop_showReadIdempotent_unsigned :: Int64 -> Bool)
    QC.quickCheck (prop_showReadIdempotent_unsigned :: Integer -> Bool)
    --
    putStrLn "SmallCheck tests."
    let d = 2 ^ (8 :: Int) :: Int
    putStrLn "Checking prop_readShowIdempotent_unsigned..."
    SC.smallCheck d (prop_readShowIdempotent_unsigned :: Int -> Bool)
    SC.smallCheck d (intBoundary prop_readShowIdempotent_unsigned)
    --
    putStrLn "Checking prop_readShowIdempotent_signed..."
    SC.smallCheck d (prop_readShowIdempotent_signed :: Int -> Bool)
    SC.smallCheck d (intBoundary prop_readShowIdempotent_signed)
    --
    putStrLn "Checking prop_showReadIdempotent_unsigned..."
    SC.smallCheck d (prop_showReadIdempotent_unsigned :: Int -> Bool)
    SC.smallCheck d (intBoundary prop_showReadIdempotent_unsigned)
    where
    -- | Test 'Integers' around the 'Int' boundary
    intBoundary :: (Integer -> Bool) -> Integer -> Bool
    intBoundary f x = f (x + fromIntegral (maxBound - 8 :: Int))

