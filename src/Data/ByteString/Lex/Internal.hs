{-# LANGUAGE ForeignFunctionInterface #-}

--------------------------------------------------------------------
-- |
-- Module    : Data.ByteString.Lex.Internal
-- Copyright : (c) Don Stewart 2008.
-- License   : BSD33
--
-- Maintainer: Don Stewart <dons00@gmail.com>
-- Stability : stable
--
-- Efficiently parse floating point literals from a ByteString
--

module Data.ByteString.Lex.Internal ( strtod, c_strtod ) where

import Data.ByteString.Internal (inlinePerformIO)
import qualified Data.ByteString as B
import Foreign.C.String (CString)
import Foreign.Ptr (Ptr, nullPtr)

-- Safe, minimal copy of substring identified by Alex.
strtod :: B.ByteString -> Double
strtod b = inlinePerformIO . B.useAsCString b $ \ptr -> c_strtod ptr nullPtr
{-# INLINE strtod #-}

foreign import ccall unsafe "stdlib.h strtod" 
    c_strtod :: CString -> Ptr CString -> IO Double
