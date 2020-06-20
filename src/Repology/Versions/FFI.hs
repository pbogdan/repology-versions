{-# LANGUAGE ForeignFunctionInterface #-}

module Repology.Versions.FFI
  ( c_version_compare2
  , c_version_compare4
  )
where

import           Foreign
import           Foreign.C.Types

foreign import ccall unsafe "libversion/version.h version_compare2"
    c_version_compare2 :: Ptr CUChar -> Ptr CUChar -> IO CInt

foreign import ccall unsafe "libversion/version.h version_compare4"
    c_version_compare4 :: Ptr CUChar -> Ptr CUChar -> CInt -> CInt -> IO CInt
