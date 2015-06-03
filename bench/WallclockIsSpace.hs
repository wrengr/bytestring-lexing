{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# LANGUAGE ForeignFunctionInterface #-}
----------------------------------------------------------------
--                                                    2012.10.28
-- |
-- Module      :  WallclockIsSpace
-- Copyright   :  Copyright (c) 2010--2012 wren gayle romano
-- License     :  BSD
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  benchmark
-- Portability :  portable (FFI)
--
-- A wall-clock benchmark for comparing different definitions of predicates for detecting whitespace.
----------------------------------------------------------------
module Main (main) where

import qualified Data.Char             as C
import           Data.Word             (Word8)
import qualified Data.ByteString.Char8 as B8
import           Foreign.C.Types       (CInt)
import qualified System.Environment    as Sys (getArgs)
import qualified System.Exit           as Sys

----------------------------------------------------------------
----- Character predicates
-- N.B. \x9..\xD == "\t\n\v\f\r"

-- | An alternate version of 'Data.Attoparsec.Char8.isSpace'.
isSpace_Char8 :: Char -> Bool
{-# INLINE isSpace_Char8 #-}
isSpace_Char8 c = (' ' == c) || ('\t' <= c && c <= '\r')

-- | An alternate version of 'isSpace_Char8' which checks in a different order.
isSpace_Char8' :: Char -> Bool
{-# INLINE isSpace_Char8' #-}
isSpace_Char8' c = (' ' == c) || (c <= '\r' && '\t' <= c)


-- | An alternate version of 'Data.Char.isSpace'. This uses the
-- same trick as 'isSpace_Char8' but we include Unicode whitespaces
-- too, in order to have the same results as 'Data.Char.isSpace'
-- (whereas 'isSpace_Char8' doesn't recognize Unicode whitespace).
isSpace_Char :: Char -> Bool
{-# INLINE isSpace_Char #-}
isSpace_Char c
    =  (' '    == c)
    || ('\t' <= c && c <= '\r')
    || ('\xA0' == c)
    || (iswspace (fromIntegral (C.ord c)) /= 0)

foreign import ccall unsafe "u_iswspace"
    iswspace :: CInt -> CInt

-- | Verbatim version of 'Data.Char.isSpace' (i.e., 'GHC.Unicode.isSpace'
-- as of base-4.2.0.2) in order to try to figure out why 'isSpace_Char'
-- is slower than 'Data.Char.isSpace'. It appears to be something
-- special in how the base library was compiled.
isSpace_DataChar :: Char -> Bool
{-# INLINE isSpace_DataChar #-}
isSpace_DataChar c =
    c == ' '     ||
    c == '\t'    ||
    c == '\n'    ||
    c == '\r'    ||
    c == '\f'    ||
    c == '\v'    ||
    c == '\xA0'  || -- -> -- BUG: syntax hilighting fail
    iswspace (fromIntegral (C.ord c)) /= 0

-- http://www.reddit.com/r/haskell/comments/124bg6/response_to_why_is_this_simple_text_processing/c6sp1hi
-- | An attempt to avoid calling 'iswspace' for ASCII non-space characters.
isSpace_fiddlosopher :: Char -> Bool
{-# INLINE isSpace_fiddlosopher #-}
isSpace_fiddlosopher c
    | c >  ' ' && c < '\xA0' = False -- -> -- BUG: syntax hilighting fail
    | c == ' '               = True
    | c >= '\t' && c <= '\r' = True
    | c == '\xA0'            = True -- -> -- BUG: syntax hilighting fail
    | otherwise              = iswspace (fromIntegral (C.ord c)) /= 0
    
-- | A variation on the theme of 'isSpace_fiddlosopher'.
isSpace_fiddlosopher' :: Char -> Bool
{-# INLINE isSpace_fiddlosopher' #-}
isSpace_fiddlosopher' c
    | c == ' '    = True
    | c <= '\r'   = '\t' <= c
    | c == '\xA0' = True   -- -> -- BUG: syntax hilighting fail
    | c <= '\xFF' = False  -- -> -- BUG: syntax hilighting fail
    | otherwise   = iswspace (fromIntegral (C.ord c)) /= 0
    -- N.B., iswspace only returns true for \x20, \xA0, and things at or above \x1680

----------------------------------------------------------------

toUpper :: Char -> Char
toUpper c
    | 'a' <= c && c <= 'z' = C.chr (C.ord c - 0x20)
    | otherwise            = c

op :: (Char -> Bool) -> Bool -> Char -> (Bool, Char)
op p b c
    | p c       = (True,  c)
    | otherwise = (False, if b then toUpper c else c)

main :: IO ()
main = do
    args <- Sys.getArgs
    case args of
        [n,fn] -> do
            let i = read n :: Int -- let it fail
            let p = ps !! i -- let it fail
            B8.readFile fn >>= B8.putStr . snd . B8.mapAccumL (op p) True
        _ -> do
            putStrLn "Must pass exactly two arguments"
            Sys.exitFailure
    where
    ps = [isSpace_Char8
        , isSpace_Char8'
        , isSpace_Char
        , isSpace_DataChar
        , C.isSpace
        , isSpace_fiddlosopher
        , isSpace_fiddlosopher'
        ]

----------------------------------------------------------------
----------------------------------------------------------- fin.
