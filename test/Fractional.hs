{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# LANGUAGE ScopedTypeVariables #-}
----------------------------------------------------------------
--                                                    2015.06.07
-- |
-- Module      :  test/Fractional
-- Copyright   :  Copyright (c) 2015 wren gayle romano
-- License     :  BSD2
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  test framework
-- Portability :  Haskell98 + ScopedTypeVariables
--
-- Correctness testing for "Data.ByteString.Lex.Fractional".
----------------------------------------------------------------
module Fractional (main) where

import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Char8        as BS8
import           Data.ByteString.Lex.Fractional
--import           Control.Monad                ((<=<))
import qualified Test.QuickCheck              as QC
--import qualified Test.SmallCheck              as SC

----------------------------------------------------------------
----------------------------------------------------------------
-- We reimplement Data.Proxy to avoid build errors on older systems

data Proxy a = Proxy 

asProxyTypeOf :: a -> Proxy a -> a
asProxyTypeOf a _ = a

----------------------------------------------------------------
-- | Fuzzy equality checking for floating-point numbers.
(=~=) :: (Fractional a, Ord a) => a -> a -> Bool
(=~=) a b = a == b || abs (a - b) <= max (abs a) (abs b) * 1e20


----------------------------------------------------------------
----- QuickCheck\/SmallCheck properties
-- N.B., these properties do not hold of 'Rational', since those
-- are shown as @numerator % denominator@.


-- | Converting a non-negative number to a string using 'show' and
-- then reading it back using 'readDecimal' returns the original
-- number.
prop_readDecimal_show
    :: (Show a, Ord a, Fractional a) => Proxy a -> Integer -> Bool
prop_readDecimal_show proxy x =
    let px = abs x in
    case (readDecimal . BS8.pack . show) px of
    Nothing         -> False
    Just (py, rest) ->
        BS.null rest && py =~= (fromInteger px `asProxyTypeOf` proxy)


-- | Converting a number to a string using 'show' and then reading
-- it back using @'readSigned' 'readDecimal'@ returns the original
-- number.
prop_readSignedDecimal_show
    :: (Show a, Ord a, Fractional a) => Proxy a -> Integer -> Bool
prop_readSignedDecimal_show proxy x =
    case (readSigned readDecimal . BS8.pack . show) x of
    Nothing        -> False
    Just (y, rest) ->
        BS.null rest && y =~= (fromInteger x `asProxyTypeOf` proxy)

----------------------------------------------------------------
-- | Converting a non-negative number to a string using 'show' and
-- then reading it back using 'readExponential' returns the original
-- number.
prop_readExponential_show :: (Show a, Ord a, Fractional a) => a -> Bool
prop_readExponential_show x =
    let px = abs x in
    case (readExponential . BS8.pack . show) px of
    Nothing         -> False
    Just (py, rest) -> BS.null rest && px =~= py


-- | Converting a number to a string using 'show' and then reading
-- it back using @'readSigned' 'readExponential'@ returns the
-- original number.
prop_readSignedExponential_show
    :: (Show a, Ord a, Fractional a) => a -> Bool
prop_readSignedExponential_show x =
    case (readSigned readExponential . BS8.pack . show) x of
    Nothing        -> False
    Just (y, rest) -> BS.null rest && x =~= y

----------------------------------------------------------------

-- | Use \"infinity\" as the precision-limit for a reader.
atInfinity
    :: (Int -> ByteString -> Maybe (a,ByteString))
    -> ByteString -> Maybe (a,ByteString)
atInfinity f = (\xs -> f (1 + BS.length xs) xs)

-- | Use a 'RealFloat' type's inherent limit as the precision-limit
-- for a reader.
atInherent
    :: forall a. RealFloat a
    => (Int -> ByteString -> Maybe (a,ByteString))
    -> ByteString -> Maybe (a,ByteString)
atInherent f = f (decimalPrecision (Proxy::Proxy a))


-- BUG: at Double, fails on 5.0e-324
--
-- | Converting a non-negative number to a string using 'show' and
-- then reading it back using 'readDecimalLimited' with an \"infinite\"
-- precision limit returns the original number.
prop_readDecimalLimitedInfinity_show
    :: (Show a, Ord a, Fractional a) => Proxy a -> Integer -> Bool
prop_readDecimalLimitedInfinity_show proxy x =
    let px = abs x in
    case (atInfinity readDecimalLimited . BS8.pack . show) px of
    Nothing         -> False
    Just (py, rest) ->
        BS.null rest && py =~= (fromInteger px `asProxyTypeOf` proxy)

-- | Converting a non-negative number to a string using 'show' and
-- then reading it back using 'readExponentialLimited' with an
-- \"infinite\" precision limit returns the original number.
prop_readExponentialLimitedInfinity_show
    :: (Show a, Ord a, Fractional a) => a -> Bool
prop_readExponentialLimitedInfinity_show x =
    let px = abs x in
    case (atInfinity readExponentialLimited . BS8.pack . show) px of
    Nothing         -> False
    Just (py, rest) -> BS.null rest && px =~= py


-- | Converting a non-negative number to a string using 'show' and
-- then reading it back using 'readDecimalLimited' with the type's
-- inherent precision limit returns the original number.
prop_readDecimalLimitedInherent_show
    :: (Show a, Ord a, RealFloat a) => Proxy a -> Integer -> Bool
prop_readDecimalLimitedInherent_show proxy x =
    let px = abs x in
    case (atInherent readDecimalLimited . BS8.pack . show) px of
    Nothing         -> False
    Just (py, rest) ->
        BS.null rest && py =~= (fromInteger px `asProxyTypeOf` proxy)

-- | Converting a non-negative number to a string using 'show' and
-- then reading it back using 'readExponentialLimited' with the
-- type's inherent precision limit returns the original number.
prop_readExponentialLimitedInherent_show
    :: (Show a, Ord a, RealFloat a) => a -> Bool
prop_readExponentialLimitedInherent_show x =
    let px = abs x in
    case (atInherent readExponentialLimited . BS8.pack . show) px of
    Nothing         -> False
    Just (py, rest) -> BS.null rest && px =~= py

----------------------------------------------------------------
----------------------------------------------------------------
main :: IO ()
main = do
    putStrLn "* Fractional tests."
    runQuickCheckTests
    runSmallCheckTests


floatProxy :: Proxy Float
floatProxy = Proxy

doubleProxy :: Proxy Double
doubleProxy = Proxy

atFloat :: (Float -> a) -> Float -> a
atFloat = id

atDouble :: (Double -> a) -> Double -> a
atDouble = id


runQuickCheckTests :: IO ()
runQuickCheckTests = do
    putStrLn "** QuickCheck tests."
    putStrLn "*** Checking prop_readDecimal_show..."
    QC.quickCheck $ prop_readDecimal_show floatProxy
    QC.quickCheck $ prop_readDecimal_show doubleProxy
    --
    putStrLn "*** Checking prop_readSignedDecimal_show..."
    QC.quickCheck $ prop_readSignedDecimal_show floatProxy
    QC.quickCheck $ prop_readSignedDecimal_show doubleProxy
    --
    putStrLn "*** Checking prop_readExponential_show..."
    QC.quickCheck $ atFloat  prop_readExponential_show
    QC.quickCheck $ atDouble prop_readExponential_show
    --
    putStrLn "*** Checking prop_readSignedExponential_show..."
    QC.quickCheck $ atFloat  prop_readSignedExponential_show
    QC.quickCheck $ atDouble prop_readSignedExponential_show
    --
    putStrLn "*** Checking prop_readDecimalLimitedInfinity_show..."
    QC.quickCheck $ prop_readDecimalLimitedInfinity_show floatProxy
    QC.quickCheck $ prop_readDecimalLimitedInfinity_show doubleProxy
    --
    putStrLn "*** Checking prop_readExponentialLimitedInfinity_show..."
    QC.quickCheck $ atFloat  prop_readExponentialLimitedInfinity_show
    QC.quickCheck $ atDouble prop_readExponentialLimitedInfinity_show
    --
    putStrLn "*** Checking prop_readDecimalLimitedInherent_show..."
    QC.quickCheck $ prop_readDecimalLimitedInherent_show floatProxy
    QC.quickCheck $ prop_readDecimalLimitedInherent_show doubleProxy
    --
    putStrLn "*** Checking prop_readExponentialLimitedInherent_show..."
    QC.quickCheck $ atFloat  prop_readExponentialLimitedInherent_show
    QC.quickCheck $ atDouble prop_readExponentialLimitedInherent_show


----------------------------------------------------------------
runSmallCheckTests :: IO ()
runSmallCheckTests = do
    -- TODO: how to properly utilize SmallCheck for this module?
    return ()

----------------------------------------------------------------
----------------------------------------------------------- fin.