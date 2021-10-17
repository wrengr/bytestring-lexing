module BenchCeilEightThirds (main) where

import Test.QuickCheck (quickCheck, (==>), Property)
import Criterion       (bench, bgroup, nf)
import Criterion.Main  (defaultMain)
import Control.Monad   (forM_)

main :: IO ()
main = do
    forM_ funs $ \(fun,f) -> do
        putStrLn $ "checking "++fun
        mapM_ (\p -> quickCheck $ guardProp p f)
            [ prop_isCeilEightThirds
            -- , prop_isNonNegative
            , prop_isNearlyPositive
            ]
    defaultMain . flip map funs $ \(fun,f) -> bench fun $ nf (map f) [0..2^15]
    where
    funs =
        [ ("naive (<805306368)",      ceilEightThirds_naive)
        , ("orig  (<268435456)",      ceilEightThirds_orig)
        , ("twan1 (<268435456)",      ceilEightThirds_twan1)
        -- This version used to be the winner back on Semiramis...
        , ("twan1-safe",              ceilEightThirds_twan1_safe)
        , ("twan2 (<268435457)",      ceilEightThirds_twan2)
        --
        -- TODO: why did this version become so fast on Ereshkigal?
        , ("makeSafe naive", makeSafe ceilEightThirds_naive)
        , ("makeSafe orig",  makeSafe ceilEightThirds_orig)
        , ("makeSafe twan1", makeSafe ceilEightThirds_twan1)
        , ("makeSafe twan2", makeSafe ceilEightThirds_twan2)
        ]

----------------------------------------------------------------
-- Unchecked, but this is what we mean here
type Nat = Int

-- | BUG: Even this will overflow on >=805306368.
ceilEightThirds_naive :: Nat -> Nat
ceilEightThirds_naive x = ceiling (fromIntegral x * 8 / 3)
    -- Oddly, if we did the division first it'd speed things up slightly.

-- N.B., 805306365 == ((maxBound::Int32) `div` 8) * 3
-- And, as we'd expect, 2^29 < 805306365 2^30
guardProp :: ((Nat -> Nat) -> Nat -> Bool) -> ((Nat -> Nat) -> Nat -> Property)
guardProp p f x = (0 <= x && x <= 805306365) ==> p f x

prop_isCeilEightThirds :: (Nat -> Nat) -> Nat -> Bool
prop_isCeilEightThirds f x = f x == ceilEightThirds_naive x

prop_isNonNegative :: (Nat -> Nat) -> Nat -> Bool
prop_isNonNegative f x = f x >= 0

prop_isNearlyPositive :: (Nat -> Nat) -> Nat -> Bool
prop_isNearlyPositive f 0 = f 0 == 0
prop_isNearlyPositive f x = f x > 0

{-# INLINE makeSafe #-}
makeSafe f x
    | x >= 805306365 = error "cannot make safe"
    | x >= 268435456 = ceilEightThirds_naive x
    | otherwise      = f x
    -- N.B. 268435456*8 becomes negative. 268435456*8-1 is the largest positive

-- | My original implementation. Fails on >=268435456.
ceilEightThirds_orig :: Nat -> Nat
ceilEightThirds_orig x
    | 0 == r    = q
    | otherwise = q+1
    where
    (q,r) = (x * 8) `quotRem` 3

-- | A better implementation by Twan van Laarhoven. Fails on >=268435456.
-- <http://kenta.blogspot.com/2012/03/umvytoye-ceildiv.html>
ceilEightThirds_twan1 :: Nat -> Nat
ceilEightThirds_twan1 x = (x*8 + 2) `quot` 3

ceilEightThirds_twan1_safe :: Nat -> Nat
ceilEightThirds_twan1_safe x
    | x >= 268435456 = ceiling (fromIntegral x * 8 / 3)
    | otherwise      = (x*8 + 2) `quot` 3

-- | Another implementation by Twan van Laarhoven. It's slower, but it can handle 268435456 correctly; though it still fails on >=268435457.
-- <http://kenta.blogspot.com/2012/03/umvytoye-ceildiv.html>
ceilEightThirds_twan2 :: Nat -> Nat
ceilEightThirds_twan2 x = negate (negate (x*8) `div` 3)

----------------------------------------------------------------
----------------------------------------------------------- fin.
