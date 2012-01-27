{
-- Turn off some common warnings about Alex-generated code.
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-missing-signatures #-}
----------------------------------------------------------------
--                                                    2012.01.25
-- |
-- Module      :  Data.ByteString.Lex.Double
-- Copyright   :  Copyright (c) 2008--2011 Don Stewart
-- License     :  BSD2/MIT
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  stable
-- Portability :  Haskell98
--
-- Efficiently parse floating point literals from a 'ByteString'.
----------------------------------------------------------------

module Data.ByteString.Lex.Double (readDouble, unsafeReadDouble) where

import qualified Data.ByteString as B
import Data.ByteString.Internal
import Data.ByteString.Lex.Internal (strtod, c_strtod)
import qualified Data.ByteString.Unsafe as B

import Foreign
import Foreign.C.Types
import Foreign.C.String
----------------------------------------------------------------
}

%wrapper "strict-bytestring"

$space       = [\ \t\xa0]
$digit       = 0-9
$octit       = 0-7
$hexit       = [$digit A-F a-f]

@sign        = [\-\+]
@decimal     = $digit+
@octal       = $octit+
@hexadecimal = $hexit+
@exponent    = [eE] [\-\+]? @decimal

@number      = @decimal
             | @decimal \. @decimal @exponent?
             | @decimal @exponent
             | 0[oO] @octal
             | 0[xX] @hexadecimal

lex :-

@sign? @number { strtod }

{

-- | Parse the initial portion of the ByteString as a Double precision
-- floating point value. The expected form of the numeric literal
-- is given by:
--
-- * An optional '+' or '-' sign  
--
-- * Decimal digits, OR
--
-- * 0 [oO] and a sequence of octal digits, OR
--
-- * 0 [xX] and a sequence of hexadecimal digits, OR
--
-- * An optional decimal point, followed by a sequence of decimal digits, 
--
-- * And an optional exponent
--
-- The result is returned as a pair of a double-precision floating
-- point value and the remaining input, or @Nothing@ should no parse
-- be found.
--
-- For example, to sum a file of floating point numbers, one per line, 
--
-- > import qualified Data.ByteString.Char8  as S
-- > import qualified Data.ByteString.Unsafe as S
-- > import Data.ByteString.Lex.Double
-- > 
-- > main = print . go 0 =<< S.getContents
-- >   where
-- >     go n s = case readDouble s of
-- >                     Nothing       -> n
-- >                     Just (k,rest) -> go (n+k) (S.tail rest)
--
readDouble :: ByteString -> Maybe (Double, ByteString)
readDouble str = case alexScan (AlexInput '\n' str) 0 of
    AlexEOF            -> Nothing
    AlexError _        -> Nothing
    AlexToken (AlexInput _ rest) n _ ->
       case strtod (B.unsafeTake n str) of d -> d `seq` Just $! (d , rest)

----------------------------------------------------------------
-- | Bare bones, unsafe wrapper for C's @strtod(3)@. This provides
-- a non-copying direct parsing of Double values from a ByteString.
-- It uses @strtod@ directly on the bytestring buffer. @strtod@
-- requires the string to be null terminated, or for a guarantee
-- that parsing will find a floating point value before the end of
-- the string.
--
unsafeReadDouble :: ByteString -> Maybe (Double, ByteString)
{-# INLINE unsafeReadDouble #-}
unsafeReadDouble b
    | B.null b  = Nothing
    | otherwise = inlinePerformIO $
        alloca $ \resptr ->
        B.unsafeUseAsCString b $ \ptr -> do -- copy just the bytes we want to parse
--          resetErrno
            d      <- c_strtod ptr resptr  -- 
--          err    <- getErrno
            newPtr <- peek resptr
            return $! case d of
                0 | newPtr == ptr -> Nothing
--              _ | err == eRANGE -> Nothing -- adds 10% overhead
                _ | otherwise  ->
                        let rest = B.unsafeDrop (newPtr `minusPtr` ptr) b
                            z    = realToFrac d
                        in z `seq` rest `seq` Just $! (z, rest)

} 
