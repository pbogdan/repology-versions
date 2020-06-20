{-# LANGUAGE ForeignFunctionInterface #-}

module Repology.Versions.FFI
  ( c_version_compare2
  )
where

import           Foreign
import           Foreign.C.Types

foreign import ccall unsafe "libversion/version.h version_compare2"
    c_version_compare2 :: Ptr CUChar -> Ptr CUChar -> IO CInt
