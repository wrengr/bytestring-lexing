{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# LANGUAGE ForeignFunctionInterface #-}
----------------------------------------------------------------
--                                                    2012.10.28
-- |
-- Module      :  BenchIsSpace
-- Copyright   :  Copyright (c) 2010--2015 wren gayle romano
-- License     :  BSD2
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  benchmark
-- Portability :  portable (FFI)
--
-- A benchmark for comparing different definitions of predicates
-- for detecting whitespace. As of the last run the results are:
--
-- * isSpace_AttoChar8 : 77.50009 us +/- 1.126696 us
-- * isSpace_DataChar  : 42.84817 us +/- 522.0762 ns
-- * isSpace_Char      : 29.27316 us +/- 387.9899 ns
-- * Data.Char.isSpace : 14.83056 us +/- 594.4243 ns
-- * isSpace_Char8     : 11.51052 us +/- 170.1964 ns
-- * isSpace_w8        : 11.42508 us +/- 154.7014 ns
-- * isSpace_w8'       : 9.980570 us +/- 161.3726 ns
--
-- * (not . isSpace_w8') : 5.827532 us +/- 68.89139 ns
-- * isNotSpace_w8       : 10.01022 us +/- 146.6599 ns
-- * isNotSpace_w8'      : 10.50834 us +/- 154.6483 ns
-- * isNotSpace_w8''     : 10.11852 us +/- 164.4095 ns
-- * not_isSpace_w8'     : 12.65621 us +/- 188.9035 ns
----------------------------------------------------------------
module BenchIsSpace (main) where

import qualified Data.Char             as C
import           Data.Word             (Word8)
import qualified Data.ByteString.Char8 as B8
import           Foreign.C.Types       (CInt(..))
import           Criterion             (bench, bgroup, nf)
import           Criterion.Main        (defaultMain)

----------------------------------------------------------------
----- Character predicates
-- N.B. \x9..\xD == "\t\n\v\f\r"

-- | Recognize the same characters as Perl's @/\s/@ in Unicode mode.
-- In particular, we recognize POSIX 1003.2 @[[:space:]]@ except
-- @\'\v\'@, and recognize the Unicode @\'\x85\'@, @\'\x2028\'@,
-- @\'\x2029\'@. Notably, @\'\x85\'@ belongs to Latin-1 (but not
-- ASCII) and therefore does not belong to POSIX 1003.2 @[[:space:]]@
-- (nor non-Unicode @/\s/@).
isPerlSpace :: Char -> Bool
{-# INLINE isPerlSpace #-}
isPerlSpace c
    =  (' '      == c)
    || ('\t' <= c && c <= '\r' && c /= '\v')
    || ('\x85'   == c)
    || ('\x2028' == c)
    || ('\x2029' == c)


-- | 'Data.Attoparsec.Char8.isSpace', duplicated here because it's
-- not exported. This is the definition as of attoparsec-0.8.1.0.
isSpace_AttoChar8 :: Char -> Bool
{-# INLINE isSpace_AttoChar8 #-}
isSpace_AttoChar8 c = c `B8.elem` spaces
    where
    {-# NOINLINE spaces #-}
    spaces = B8.pack " \n\r\t\v\f"


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

-- | A 'Word8' version of 'isSpace_Char8'.
isSpace_w8 :: Word8 -> Bool
{-# INLINE isSpace_w8 #-}
isSpace_w8 w = (w == 32) || (9 <= w && w <= 13)


-- | An alternate version of 'isSpace_w8' which checks in a different order.
isSpace_w8' :: Word8 -> Bool
{-# INLINE isSpace_w8' #-}
isSpace_w8' w = (w == 0x20) || (w <= 0x0D && 0x09 <= w)
-- 0x09..0x0D,0x20 == "\t\n\v\f\r "



-- | The negation of 'isSpace_w8''. But this (and all alternatives) appear to be a lot slower than the obvious implementation...
isNotSpace_w8 :: Word8 -> Bool
{-# INLINE isNotSpace_w8 #-}
isNotSpace_w8 w = (0x0D < w && w /= 0x20) || (w < 0x09)

{-
    not ((w <= 0x0D && 0x09 <= w) || (w == 0x20))
    ===
    not (w <= 0x0D && 0x09 <= w) && not (w == 0x20)
    ===
    not (w <= 0x0D && 0x09 <= w) && (w /= 0x20)
    ===
    (not (w <= 0x0D) || not (0x09 <= w)) && (w /= 0x20)
    ===
    (w > 0x0D || 0x09 > w) && (w /= 0x20)
    ===
    (w > 0x0D && w /= 0x20) || (0x09 > w)
-}
isNotSpace_w8' :: Word8 -> Bool
{-# INLINE isNotSpace_w8' #-}
isNotSpace_w8' w = (w > 0x0D || 0x09 > w) && (w /= 0x20)

isNotSpace_w8'' :: Word8 -> Bool
{-# INLINE isNotSpace_w8'' #-}
isNotSpace_w8'' w = (w > 0x0D && w /= 0x20) || (0x09 > w)

-- | The obvious implementation (out-lined) is even slower. Something's up...
not_isSpace_w8' :: Word8 -> Bool
{-# INLINE not_isSpace_w8' #-}
not_isSpace_w8' = not . isSpace_w8'

----------------------------------------------------------------

main :: IO ()
main = defaultMain
    [ bgroup "isSpace@Char"
        [ bench "Data.Char.isSpace" $ nf (map C.isSpace)        ['\x0'..'\255']
        , bench "isSpace_DataChar"  $ nf (map isSpace_DataChar) ['\x0'..'\255']
        , bench "isSpace_Char"      $ nf (map isSpace_Char)     ['\x0'..'\255']
        -- , bench "isPerlSpace"       $ nf (map isPerlSpace)   ['\x0'..'\255']
        , bench "isSpace_AttoChar8" $ nf (map isSpace_AttoChar8)['\x0'..'\255']
        , bench "isSpace_Char8"     $ nf (map isSpace_Char8)    ['\x0'..'\255']
        , bench "isSpace_Char8'"    $ nf (map isSpace_Char8')   ['\x0'..'\255']
        , bench "isSpace_fiddlosopher" $ nf (map isSpace_fiddlosopher) ['\x0'..'\255']
        , bench "isSpace_fiddlosopher'" $ nf (map isSpace_fiddlosopher') ['\x0'..'\255']
        ]
    , bgroup "isSpace@Word8"
        [ bench "isSpace_w8"        $ nf (map isSpace_w8)       [0..255]
        , bench "isSpace_w8'"       $ nf (map isSpace_w8')      [0..255]
        ]
    , bgroup "isNotSpace@Word8"
        [ bench "not . isSpace_w8'" $ nf (map (not . isSpace_w8')) [0..255]
        , bench "isNotSpace_w8"     $ nf (map isNotSpace_w8)       [0..255]
        , bench "isNotSpace_w8'"    $ nf (map isNotSpace_w8')      [0..255]
        , bench "isNotSpace_w8''"   $ nf (map isNotSpace_w8'')     [0..255]
        , bench "not_isSpace_w8'"   $ nf (map not_isSpace_w8')     [0..255]
        ]
    ]

{- -- https://gist.github.com/3967761
sophocles :: String
sophocles = "Ἰοὺ ἰού· τὰ πάντʼ ἂν ἐξήκοι σαφῆ.\nὮ φῶς, τελευταῖόν σε προσϐλέψαιμι νῦν,\nὅστις πέφασμαι φύς τʼ ἀφʼ ὧν οὐ χρῆν, ξὺν οἷς τʼ\nοὐ χρῆν ὁμιλῶν, οὕς τέ μʼ οὐκ ἔδει κτανών.\nἸοὺ ἰού· τὰ πάντʼ ἂν ἐξήκοι σαφῆ.\nὮ φῶς, τελευταῖόν σε προσϐλέψαιμι νῦν,\nὅστις πέφασμαι φύς τʼ ἀφʼ ὧν οὐ χρῆν, ξὺν οἷς τʼ\nοὐ χρῆν ὁμιλῶν, οὕς τέ μʼ οὐκ ἔδει κτανών."

lorem :: String
lorem = "Lorem ipsum dolor sit amet, consectetur adipisicing elit,\nsed do eiusmod tempor incididunt ut labore et\ndolore magna aliqua. Ut enim ad minim veniam,\nquis nostrud exercitation ullamco laboris nisi ut\naliquip ex ea commodo consequat. Duis aute irure dolor\nin reprehenderit in voluptate velit esse cillum dolore\neu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident,\nsunt in culpa qui officia deserunt mollit anim id est laborum.\n"

haskell :: String
haskell = "isSpace_DataChar :: Char -> Bool\n{-# INLINE isSpace_DataChar #-}\n  isSpace_DataChar c =\n     c == ' '     ||\n     c == '\t'    ||\n     c == '\n'    ||\n     c == '\r'    ||\n     c == '\f'    ||\n\tc == '\v'    ||\n\tc == '\xa0'  ||\n\tiswspace (fromIntegral (C.ord c)) /= 0\nisSpace_DataChar :: Char -> Bool\n{-# INLINE isSpace_DataChar #-}\n  isSpace_DataChar c =\n     c == ' '     ||\n     c == '\t'    ||\n     c == '\n'    ||\n     c == '\r'    ||\n     c == '\f'    ||\n\tc == '\v'    ||\n\tc == '\xa0'  ||\n\tiswspace (fromIntegral (C.ord c)) /= 0\n"

main :: IO ()
main = defaultMain
    [ group "ascii text" lorem
    , group "Greek text" sophocles
    , group "Haskell code" haskell
    , group "chars 0..255" ['\0'..'\255']
    , group "all spaces" $ concat $ replicate 50 "\t\r\n\n "
    ]

group :: String -> String -> Benchmark
group name inp = bgroup name
        [ bench "isSpace_DataChar" $ nf (map isSpace_DataChar) inp
        , bench "isSpace_Pattern"  $ nf (map isSpace_Pattern)  inp
        , bench "isSpace_Alt"      $ nf (map isSpace_Alt)      inp
        , bench "isSpace_Alt'"     $ nf (map isSpace_Alt')     inp
        ]
-}

----------------------------------------------------------------
----------------------------------------------------------- fin.
