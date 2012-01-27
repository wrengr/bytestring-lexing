{
-- Turn off some common warnings about Alex-generated code.
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-missing-signatures #-}
----------------------------------------------------------------
--                                                    2012.01.25
-- |
-- Module      :  Data.ByteString.Lex.Lazy.Double
-- Copyright   :  Copyright (c) 2008--2011 Don Stewart
-- License     :  BSD2/MIT
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  stable
-- Portability :  Haskell98
--
-- Efficiently parse floating point literals from a 'ByteString'.
----------------------------------------------------------------

module Data.ByteString.Lex.Lazy.Double (readDouble) where

import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString      as SB
import Data.ByteString.Lex.Internal (strtod)
----------------------------------------------------------------
}

%wrapper "basic-bytestring"

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

@sign? @number { strtod . strict }

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
readDouble :: LB.ByteString -> Maybe (Double, LB.ByteString)
readDouble str = case alexScan ('\n', str) 0 of
    AlexEOF            -> Nothing
    AlexError _        -> Nothing
    AlexToken (_, rest) n _ ->
       case strtod . strict . LB.take (fromIntegral n) $ str of
         d -> Just $! (d , rest)

strict = SB.concat . LB.toChunks
}
