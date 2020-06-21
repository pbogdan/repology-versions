module Repology.Versions
  ( Version(..)
  , versionCompare
  , CompareOption
  , versionCompareWithOpts
  )
where

import qualified Data.ByteString               as Bytes
import qualified Data.ByteString.Char8         as CharBytes
import           Foreign.Ptr                    ( castPtr )
import           Repology.Versions.FFI          ( c_version_compare2
                                                , c_version_compare4
                                                )
import           System.IO.Unsafe               ( unsafePerformIO )

newtype Version = Version String deriving (Show)

instance Eq Version where
  v1 == v2 = versionCompare v1 v2 == EQ

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

data CompareOption =
  None
  | PIsPatch
  | AnyIsPatch
  | LowerBound
  | UpperBound
  deriving (Eq, Show)

instance Enum CompareOption where
  toEnum n | n == 0    = None
           | n == 1    = PIsPatch
           | n == 2    = AnyIsPatch
           | n == 4    = LowerBound
           | n == 8    = UpperBound
           | otherwise = error "CompareFlag.toEnum: invalid argument"

  fromEnum None       = 0
  fromEnum PIsPatch   = 1
  fromEnum AnyIsPatch = 2
  fromEnum LowerBound = 4
  fromEnum UpperBound = 8

versionCompareWithOpts' :: String -> String -> Int -> Int -> IO Int
versionCompareWithOpts' v1 v2 o1 o2 =
  Bytes.useAsCString (CharBytes.pack v1) $ \v1ptr ->
    Bytes.useAsCString (CharBytes.pack v2) $ \v2ptr ->
      fromIntegral <$> c_version_compare4 (castPtr v1ptr)
                                          (castPtr v2ptr)
                                          (fromIntegral o1)
                                          (fromIntegral o2)

versionCompareWithOpts
  :: Version -> Version -> CompareOption -> CompareOption -> Ordering
versionCompareWithOpts (Version v1) (Version v2) o1 o2 =
  let ret = unsafePerformIO
        $ versionCompareWithOpts' v1 v2 (fromEnum o1) (fromEnum o2)
  in  case ret of
        -1 -> LT
        0  -> EQ
        1  -> GT
        _  -> error "unexpected return value from c_version_compare4"
