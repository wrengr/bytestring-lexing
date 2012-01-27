{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# LANGUAGE OverloadedStrings, MagicHash, BangPatterns #-}
----------------------------------------------------------------
--                                                    2012.01.26
-- |
-- Copyright     : Erik de Castro Lopo <erikd@mega-nerd.com>
-- License       : BSD3
--
-- Originally a program to QuickCheck and benchmark a function used
-- in the Warp web server and elsewhere to read the Content-Length
-- field of HTTP headers. This version modified for testing the
-- local package.
--
-- Compile and run as:
--    @ghc -O2 --make readDecimal.hs -o readDecimal && ./readDecimal@
----------------------------------------------------------------

import Criterion.Main
import Data.ByteString (ByteString)
import Data.Int        (Int64)
import Data.Maybe      (fromMaybe)

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Unsafe   as BSU
import qualified Data.Char             as C
import qualified Numeric               as N
import qualified Test.QuickCheck       as QC

-- ReadInt is a module internal to Warp.
----import qualified ReadInt as RI

import qualified Data.ByteString.Lex.Integral as BSLex (readDecimal)

import GHC.Prim
import GHC.Types
import GHC.Word
----------------------------------------------------------------

-- This is the absolute mimimal solution. It will return garbage
-- if the input string contains anything other than ASCII digits.
readIntOrig :: ByteString -> Integer
readIntOrig = BS.foldl' (\x w -> x * 10 + fromIntegral w - 48) 0


-- Using Numeric.readDec which works on String, so the ByteString
-- has to be unpacked first.
readDec :: ByteString -> Integer
readDec s =
    case N.readDec (BS8.unpack s) of
    []      -> 0
    (x,_):_ -> x


-- Use ByteString's readInteger function.
readIntegerBS :: ByteString -> Integer
readIntegerBS = fst . fromMaybe (0,"") . BS8.readInteger


-- No checking for non-digits. Will overflow at 2^31 on 32 bit CPUs.
readIntRaw :: ByteString -> Int
readIntRaw = BS8.foldl' (\i c -> i * 10 + C.digitToInt c) 0

----------------------------------------------------------------
-- The best solution Erik found.
readIntTC :: Integral a => ByteString -> a
readIntTC
    = fromIntegral
    . BS8.foldl' (\i c -> i * 10 + C.digitToInt c) 0 
    . BS8.takeWhile C.isDigit

-- Three specialisations of readIntTC.
readInt :: ByteString -> Int
readInt = readIntTC

readInt64 :: ByteString -> Int64
readInt64 = readIntTC

readInteger :: ByteString -> Integer
readInteger = readIntTC

----------------------------------------------------------------
-- MagicHash version suggested by Vincent Hanquez.
readIntegralMH :: Integral a => ByteString -> a
readIntegralMH = fromIntegral . ireadInt64MH
    where
    ireadInt64MH :: ByteString -> Int64
    ireadInt64MH
        = BS8.foldl' (\i c -> i * 10 + fromIntegral (mhDigitToInt c)) 0
        . BS8.takeWhile C.isDigit

readIntMH :: ByteString -> Int
readIntMH = readIntegralMH

readInt64MH :: ByteString -> Int64
readInt64MH = readIntegralMH

readIntegerMH :: ByteString -> Integer
readIntegerMH = readIntegralMH

data Table = Table !Addr#

mhDigitToInt :: Char -> Int
mhDigitToInt (C# i) = I# (word2Int# (indexWord8OffAddr# addr (ord# i)))
    where
    !(Table addr) = table
    table :: Table
    table = Table
        "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#
-- -> -- Fix a syntax highlighting bug in jEdit.

----------------------------------------------------------------
-- A faster MagicHash version by Christoph Breitkopf
readIntegralMHFast :: Integral a => ByteString -> a
readIntegralMHFast s = go 0 0 (BS8.length s) s
    where
    go :: Integral a => a -> Int -> Int -> ByteString -> a
    go n i len bs
        | n `seq` i `seq` len `seq` bs `seq` False = undefined
        | i >= len  = n
        | v < 10    = go (10 * n + v) (i+1) len bs
        | otherwise = n
        where
        v = fromIntegral (mhDigitToIntFast (BS8.index bs i))

mhDigitToIntFast :: Char -> Int
mhDigitToIntFast (C# i) = I# (word2Int# (indexWord8OffAddr# addr (ord# i)))
    where
    !(Table addr) = table
    table :: Table
    table = Table
        "\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\
        \\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\
        \\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\
        \\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x7f\x7f\x7f\x7f\x7f\x7f\
        \\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\
        \\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\
        \\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\
        \\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\
        \\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\
        \\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\
        \\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\
        \\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\
        \\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\
        \\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\
        \\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\
        \\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f"#
-- -> -- Fix a syntax highlighting bug in jEdit.

readIntMHFast :: ByteString -> Int
readIntMHFast = readIntegralMHFast

readInt64MHFast :: ByteString -> Int64
readInt64MHFast = readIntegralMHFast

readIntegerMHFast :: ByteString -> Integer
readIntegerMHFast = readIntegralMHFast

----------------------------------------------------------------
-- Crank on it just a bit more
readIntegralMHFast' :: Integral a => ByteString -> a
readIntegralMHFast' s = go 0 0 s
    where
    len = BS8.length s
    
    go :: Integral a => a -> Int -> ByteString -> a
    go n i bs
        | n `seq` i `seq` bs `seq` False = undefined
        | i >= len  = n
        | v < 10    = go (10 * n + v) (i+1) bs
        | otherwise = n
        where
        v = fromIntegral (mhDigitToIntFast' (BSU.unsafeIndex bs i))

mhDigitToIntFast' :: Word8 -> Int
mhDigitToIntFast' (W8# i) = I# (indexInt8OffAddr# addr (word2Int# i))
    where
    !(Table addr) = table
    table :: Table
    table = Table
        "\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\
        \\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\
        \\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\
        \\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x7f\x7f\x7f\x7f\x7f\x7f\
        \\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\
        \\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\
        \\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\
        \\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\
        \\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\
        \\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\
        \\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\
        \\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\
        \\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\
        \\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\
        \\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\
        \\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f\x7f"#
-- -> -- Fix a syntax highlighting bug in jEdit.

readIntMHFast' :: ByteString -> Int
readIntMHFast' = readIntegralMHFast'

readInt64MHFast' :: ByteString -> Int64
readInt64MHFast' = readIntegralMHFast'

readIntegerMHFast' :: ByteString -> Integer
readIntegerMHFast' = readIntegralMHFast'

----------------------------------------------------------------
-- This is the one Warp actually uses. It's essentially the same as readInt64MH
----readIntWarp :: ByteString -> Integer
----readIntWarp s = fromIntegral $ RI.readInt64 s

----------------------------------------------------------------
readDecimalInt_      :: ByteString -> Int
readDecimalInt_      = fst . fromMaybe (0,"") . BSLex.readDecimal

readDecimalInt64_    :: ByteString -> Int64
readDecimalInt64_    = fst . fromMaybe (0,"") . BSLex.readDecimal

readDecimalInt64_2   :: ByteString -> Int64
readDecimalInt64_2   = fromIntegral . readDecimalInt_

readDecimalInteger_  :: ByteString -> Integer
readDecimalInteger_  = fst . fromMaybe (0,"") . BSLex.readDecimal

readDecimalInteger_2 :: ByteString -> Integer
readDecimalInteger_2 = fromIntegral . readDecimalInt_


-- This splits the difference between the slow but correct
-- 'readDecimalInt64_' (840ns) and the fast but incorrect
-- 'readDecimalInt64_2' (175ns) at about 475ns on my machine. N.B.,
-- passing a parameter to track our position in the group reduces
-- performance down to the level of 'readIntOrig' and 'readIntegerBS';
-- so the code duplication of unrolling the loop seems necessary
-- for this approach.
readDecimalInt64_3 :: ByteString -> Int64
readDecimalInt64_3 = fst . fromMaybe (0,"") . start
    where
    start xs
        | BS.null xs = Nothing
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    Just $ loop1 0 (fromIntegral(w-0x30)) (BSU.unsafeTail xs)
              | otherwise -> Nothing
    
    loop0 :: Int64 -> ByteString -> (Int64, ByteString)
    loop1, loop2, loop3, loop4, loop5, loop6, loop7, loop8 :: Int64 -> Int -> ByteString -> (Int64, ByteString)
    loop0 m xs
        | m `seq` xs `seq` False = undefined
        | BS.null xs = (m, BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    loop1 m (fromIntegral(w-0x30)) (BSU.unsafeTail xs)
              | otherwise -> (m, xs)
    loop1 m n xs
        | m `seq` n `seq` xs `seq` False = undefined
        | BS.null xs = (m*10 + fromIntegral n, BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    loop2 m (n*10 + fromIntegral(w-0x30)) (BSU.unsafeTail xs)
              | otherwise -> (m*10 + fromIntegral n, xs)
    loop2 m n xs
        | m `seq` n `seq` xs `seq` False = undefined
        | BS.null xs = (m*100 + fromIntegral n, BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    loop3 m (n*10 + fromIntegral(w-0x30)) (BSU.unsafeTail xs)
              | otherwise -> (m*100 + fromIntegral n, xs)
    loop3 m n xs
        | m `seq` n `seq` xs `seq` False = undefined
        | BS.null xs = (m*1000 + fromIntegral n, BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    loop4 m (n*10 + fromIntegral(w-0x30)) (BSU.unsafeTail xs)
              | otherwise -> (m*1000 + fromIntegral n, xs)
    loop4 m n xs
        | m `seq` n `seq` xs `seq` False = undefined
        | BS.null xs = (m*10000 + fromIntegral n, BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    loop5 m (n*10 + fromIntegral(w-0x30)) (BSU.unsafeTail xs)
              | otherwise -> (m*10000 + fromIntegral n, xs)
    loop5 m n xs
        | m `seq` n `seq` xs `seq` False = undefined
        | BS.null xs = (m*100000 + fromIntegral n, BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    loop6 m (n*10 + fromIntegral(w-0x30)) (BSU.unsafeTail xs)
              | otherwise -> (m*100000 + fromIntegral n, xs)
    loop6 m n xs
        | m `seq` n `seq` xs `seq` False = undefined
        | BS.null xs = (m*1000000 + fromIntegral n, BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    loop7 m (n*10 + fromIntegral(w-0x30)) (BSU.unsafeTail xs)
              | otherwise -> (m*1000000 + fromIntegral n, xs)
    loop7 m n xs
        | m `seq` n `seq` xs `seq` False = undefined
        | BS.null xs = (m*10000000 + fromIntegral n, BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    loop8 m (n*10 + fromIntegral(w-0x30)) (BSU.unsafeTail xs)
              | otherwise -> (m*10000000 + fromIntegral n, xs)
    loop8 m n xs
        | m `seq` n `seq` xs `seq` False = undefined
        | BS.null xs = (m*100000000 + fromIntegral n, BS.empty)
        | otherwise  =
            case BSU.unsafeHead xs of
            w | 0x39 >= w && w >= 0x30 ->
                    loop0 (m*1000000000 + fromIntegral (n*10 + fromIntegral(w-0x30))) (BSU.unsafeTail xs)
              | otherwise -> (m*100000000 + fromIntegral n, xs)

----------------------------------------------------------------
-- A QuickCheck property. Test that for a number >= 0, converting it to
-- a string using show and then reading the value back with the function
-- under test returns the original value.
-- The functions under test only work on Natural numbers (the Conent-Length
-- field in a HTTP header is always >= 0) so we check the absolute value of
-- the value that QuickCheck generates for us.
prop_read_show_idempotent :: Integral a => (ByteString -> a) -> a -> Bool
prop_read_show_idempotent freader x =
    let px = abs x
    in px == freader (BS8.pack $ show px)


runQuickCheckTests :: IO ()
runQuickCheckTests = do
    putStrLn "Checking readInt..."
    QC.quickCheck (prop_read_show_idempotent readInt)
    putStrLn "Checking readInt64..."
    QC.quickCheck (prop_read_show_idempotent readInt64)
    putStrLn "Checking readInteger..."
    QC.quickCheck (prop_read_show_idempotent readInteger)
    --
    putStrLn "Checking readIntMH..."
    QC.quickCheck (prop_read_show_idempotent readIntMH)
    putStrLn "Checking readInt64MH..."
    QC.quickCheck (prop_read_show_idempotent readInt64MH)
    putStrLn "Checking readIntegerMH..."
    QC.quickCheck (prop_read_show_idempotent readIntegerMH)
    --
    putStrLn "Checking readIntMHFast..."
    QC.quickCheck (prop_read_show_idempotent readIntMHFast)
    putStrLn "Checking readInt64MHFast..."
    QC.quickCheck (prop_read_show_idempotent readInt64MHFast)
    putStrLn "Checking readIntegerMHFast..."
    QC.quickCheck (prop_read_show_idempotent readIntegerMHFast)
    --
    putStrLn "Checking readIntMHFast'..."
    QC.quickCheck (prop_read_show_idempotent readIntMHFast')
    putStrLn "Checking readInt64MHFast'..."
    QC.quickCheck (prop_read_show_idempotent readInt64MHFast')
    putStrLn "Checking readIntegerMHFast'..."
    QC.quickCheck (prop_read_show_idempotent readIntegerMHFast')
    --
    ----putStrLn "Checking readIntWarp..."
    ----QC.quickCheck (prop_read_show_idempotent readIntWarp)
    --
    putStrLn "Checking readDecimalInt_..."
    QC.quickCheck (prop_read_show_idempotent readDecimalInt_)
    putStrLn "Checking readDecimalInt64_..."
    QC.quickCheck (prop_read_show_idempotent readDecimalInt64_)
    putStrLn "Checking readDecimalInt64_2..."
    QC.quickCheck (prop_read_show_idempotent readDecimalInt64_2)
    putStrLn "Checking readDecimalInt64_3..."
    QC.quickCheck (prop_read_show_idempotent readDecimalInt64_3)
    putStrLn "Checking readDecimalInteger_..."
    QC.quickCheck (prop_read_show_idempotent readDecimalInteger_)
    putStrLn "Checking readDecimalInteger_2..."
    QC.quickCheck (prop_read_show_idempotent readDecimalInteger_2)


runCriterionTests :: ByteString -> IO ()
runCriterionTests number =
    defaultMain
        [ bgroup "naive"
            [ bench "readIntOrig"       $ nf readIntOrig number
            , bench "readDec"           $ nf readDec number
            , bench "readIntegerBS"     $ nf readIntegerBS number
            , bench "readRaw"           $ nf readIntRaw number
            ]
        , bgroup "readIntTC"
            [ bench "readInt"           $ nf readInt number
            , bench "readInt64"         $ nf readInt64 number
            , bench "readInteger"       $ nf readInteger number
            ]
        , bgroup "readIntegralMH"
            [ bench "readIntMH"         $ nf readIntMH number
            , bench "readInt64MH"       $ nf readInt64MH number
            , bench "readIntegerMH"     $ nf readIntegerMH number
            ]
        , bgroup "readIntegralMHFast"
            [ bench "readIntMHFast"     $ nf readIntMHFast number
            , bench "readInt64MHFast"   $ nf readInt64MHFast number
            , bench "readIntegerMHFast" $ nf readIntegerMHFast number
            ]
        , bgroup "readIntegralMHFast'"
            [ bench "readIntMHFast'"     $ nf readIntMHFast' number
            , bench "readInt64MHFast'"   $ nf readInt64MHFast' number
            , bench "readIntegerMHFast'" $ nf readIntegerMHFast' number
            ]
        ---- , bench "readIntWarp"   $ nf readIntWarp number
        , bgroup "readDecimal (correct)"
            [ bench "readDecimalInt_"      $ nf readDecimalInt_ number
            , bench "readDecimalInt64_"    $ nf readDecimalInt64_ number
            , bench "readDecimalInteger_"  $ nf readDecimalInteger_ number
            ]
        , bgroup "readDecimal (buggy fast)"
            [ bench "readDecimalInt64_2"   $ nf readDecimalInt64_2 number
            , bench "readDecimalInteger_2" $ nf readDecimalInteger_2 number
            ]
        , bgroup "readDecimal (unrolled)"
            [ bench "readDecimalInt64_3"   $ nf readDecimalInt64_3 number
            ]
        ]

main :: IO ()
main = do
    putStrLn "Quickcheck tests."
    runQuickCheckTests
    putStrLn "Criterion tests."
    runCriterionTests "1234567898765432178979128361238162386182"

