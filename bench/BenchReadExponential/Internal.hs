{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# LANGUAGE ForeignFunctionInterface #-}
----------------------------------------------------------------
--                                                    2012.01.25
-- |
-- Module      :  Data.ByteString.Lex.Internal
-- Copyright   :  Copyright (c) 2008--2011 Don Stewart.
-- License     :  BSD2/MIT
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  stable
-- Portability :  Haskell98 + FFI
--
-- Efficiently parse floating point literals from a 'ByteString'.
----------------------------------------------------------------

module BenchReadExponential.Internal (strtod, c_strtod) where

import qualified Data.ByteString.Internal as BSI (inlinePerformIO)
import qualified Data.ByteString          as BS
import           Foreign.C.String         (CString)
import           Foreign.Ptr              (Ptr, nullPtr)
----------------------------------------------------------------

-- | Safe, minimal copy of substring identified by Alex.
strtod :: BS.ByteString -> Double
{-# INLINE strtod #-}
strtod b =
    BSI.inlinePerformIO . BS.useAsCString b $ \ptr -> c_strtod ptr nullPtr

foreign import ccall unsafe "stdlib.h strtod" 
    c_strtod :: CString -> Ptr CString -> IO Double

----------------------------------------------------------------
----------------------------------------------------------- fin.