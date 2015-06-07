{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
----------------------------------------------------------------
--                                                    2015.06.07
-- |
-- Module      :  test/Main
-- Copyright   :  Copyright (c) 2015 wren gayle romano
-- License     :  BSD2
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  benchmark
-- Portability :  Haskell98
--
-- Run all the basic correctness tests.
----------------------------------------------------------------
module Main (main) where
-- import Distribution.TestSuite (Test)
import qualified Integral
import qualified Fractional

----------------------------------------------------------------
----------------------------------------------------------------

-- N.B., this will always exit with 0 and therefore count as "success"
-- in TravisCI
main :: IO ()
main = do
    Integral.main
    Fractional.main

{-
-- TODO: get this to work. Or something similar with test-framework or tasty
tests :: IO [Test]
tests = do
    integralTests   <- Integral.tests
    fractionalTests <- Fractional.tests
    return (integralTests ++ fractionalTests)
-}

----------------------------------------------------------------
----------------------------------------------------------- fin.
