module Repology.Versions
  ( Version(..)
  , versionCompare
  )
where

import qualified Data.ByteString               as Bytes
import qualified Data.ByteString.Char8         as CharBytes
import           Foreign.Ptr                    ( castPtr )
import           Repology.Versions.FFI          ( c_version_compare2 )
import           System.IO.Unsafe               ( unsafePerformIO )

newtype Version = Version String deriving (Eq, Show)

instance Ord Version where
  compare = versionCompare

versionCompare' :: String -> String -> IO Int
versionCompare' v1 v2 = Bytes.useAsCString (CharBytes.pack v1) $ \v1ptr ->
  Bytes.useAsCString (CharBytes.pack v2) $ \v2ptr ->
    fromIntegral <$> c_version_compare2 (castPtr v1ptr) (castPtr v2ptr)

versionCompare :: Version -> Version -> Ordering
versionCompare (Version v1) (Version v2) =
  let ret = unsafePerformIO $ versionCompare' v1 v2
  in  case ret of
        -1 -> LT
        0  -> EQ
        1  -> GT
        _  -> error "unexpected return value from c_version_compare2"
