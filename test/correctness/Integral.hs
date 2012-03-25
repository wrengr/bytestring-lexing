{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
----------------------------------------------------------------
--                                                    2012.03.24
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
import           Control.Monad                ((<=<))
import qualified Test.QuickCheck              as QC
import qualified Test.SmallCheck              as SC

----------------------------------------------------------------
----- QuickCheck\/SmallCheck properties

-- | Converting a non-negative number to a string using 'show' and
-- then reading it back using 'readDecimal' returns the original
-- number.
prop_readDecimal_show :: (Show a, Integral a) => a -> Bool
prop_readDecimal_show x =
    let px = abs x
    in  Just (px, BS.empty) == (readDecimal . BS8.pack . show) px


-- | Converting a number to a string using 'show' and then reading
-- it back using @'readSigned' 'readDecimal'@ returns the original
-- number.
prop_readSignedDecimal_show :: (Show a, Integral a) => a -> Bool
prop_readSignedDecimal_show x =
    Just (x, BS.empty) == (readSigned readDecimal . BS8.pack . show) x


-- | Converting a non-negative number to a string using 'show' and
-- then reading it back using 'readDecimal_' returns the original
-- number.
prop_readDecimalzu_show :: (Show a, Integral a) => a -> Bool
prop_readDecimalzu_show x =
    let px = abs x
    in  px == (readDecimal_ . BS8.pack . show) px


-- | Converting a non-negative number to a bytestring using 'packDecimal' and then reading it back using 'read' returns the original number.
prop_read_packDecimal :: (Read a, Integral a) => a -> Bool
prop_read_packDecimal x =
    let px = abs x
    in  px == (read . maybe "" BS8.unpack . packDecimal) px

----------------------------------------------------------------
-- | Converting a non-negative number to a string using 'packHexadecimal' and then reading it back using 'readHexadecimal' returns the original number.
prop_readHexadecimal_packHexadecimal :: (Show a, Integral a) => a -> Bool
prop_readHexadecimal_packHexadecimal x =
    let px = abs x
    in  Just (px, BS.empty) == (readHexadecimal <=< packHexadecimal) px

-- TODO: how can we check the other composition with QC/SC?

----------------------------------------------------------------
-- | Converting a non-negative number to a string using 'packOctal' and then reading it back using 'readOctal' returns the original number.
prop_readOctal_packOctal :: (Show a, Integral a) => a -> Bool
prop_readOctal_packOctal x =
    let px = abs x
    in  Just (px, BS.empty) == (readOctal <=< packOctal) px

-- TODO: how can we check the other composition with QC/SC?

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
    putStrLn "Checking prop_readDecimal_show..."
    QC.quickCheck (prop_readDecimal_show :: Int -> Bool)
    QC.quickCheck (prop_readDecimal_show :: Int64 -> Bool)
    QC.quickCheck (prop_readDecimal_show :: Integer -> Bool)
    --
    putStrLn "Checking prop_readDecimalzu_show..."
    QC.quickCheck (prop_readDecimalzu_show :: Int -> Bool)
    QC.quickCheck (prop_readDecimalzu_show :: Int64 -> Bool)
    QC.quickCheck (prop_readDecimalzu_show :: Integer -> Bool)
    --
    putStrLn "Checking prop_readSignedDecimal_show..."
    QC.quickCheck (prop_readSignedDecimal_show :: Int -> Bool)
    QC.quickCheck (prop_readSignedDecimal_show :: Int64 -> Bool)
    QC.quickCheck (prop_readSignedDecimal_show :: Integer -> Bool)
    --
    putStrLn "Checking prop_read_packDecimal..."
    QC.quickCheck (prop_read_packDecimal :: Int -> Bool)
    QC.quickCheck (prop_read_packDecimal :: Int64 -> Bool)
    QC.quickCheck (prop_read_packDecimal :: Integer -> Bool)
    --
    putStrLn "Checking prop_readHexadecimal_packHexadecimal..."
    QC.quickCheck (prop_readHexadecimal_packHexadecimal :: Int -> Bool)
    QC.quickCheck (prop_readHexadecimal_packHexadecimal :: Int64 -> Bool)
    QC.quickCheck (prop_readHexadecimal_packHexadecimal :: Integer -> Bool)
    --
    putStrLn "Checking prop_readOctal_packOctal..."
    QC.quickCheck (prop_readOctal_packOctal :: Int -> Bool)
    QC.quickCheck (prop_readOctal_packOctal :: Int64 -> Bool)
    QC.quickCheck (prop_readOctal_packOctal :: Integer -> Bool)
    --
    putStrLn "SmallCheck tests."
    let d = 2 ^ (8 :: Int) :: Int
    putStrLn "Checking prop_readDecimal_show..."
    SC.smallCheck d (prop_readDecimal_show :: Int -> Bool)
    SC.smallCheck d (intBoundary prop_readDecimal_show)
    --
    putStrLn "Checking prop_readDecimalzu_show..."
    SC.smallCheck d (prop_readDecimalzu_show :: Int -> Bool)
    SC.smallCheck d (intBoundary prop_readDecimalzu_show)
    --
    putStrLn "Checking prop_readSignedDecimal_show..."
    SC.smallCheck d (prop_readSignedDecimal_show :: Int -> Bool)
    SC.smallCheck d (intBoundary prop_readSignedDecimal_show)
    --
    putStrLn "Checking prop_read_packDecimal..."
    SC.smallCheck d (prop_read_packDecimal :: Int -> Bool)
    SC.smallCheck d (intBoundary prop_read_packDecimal)
    --
    putStrLn "Checking prop_readHexadecimal_packHexadecimal..."
    SC.smallCheck d (prop_readHexadecimal_packHexadecimal :: Int -> Bool)
    SC.smallCheck d (intBoundary prop_readHexadecimal_packHexadecimal)
    --
    putStrLn "Checking prop_readOctal_packOctal..."
    SC.smallCheck d (prop_readOctal_packOctal :: Int -> Bool)
    SC.smallCheck d (intBoundary prop_readOctal_packOctal)
    where
    -- | Test 'Integers' around the 'Int' boundary
    intBoundary :: (Integer -> Bool) -> Integer -> Bool
    intBoundary f x = f (x + fromIntegral (maxBound - 8 :: Int))

