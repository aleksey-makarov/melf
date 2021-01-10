{-# LANGUAGE FlexibleInstances #-}

module Data.Endian (Be(..), Le(..)) where

import Data.Binary.Put
import Data.Binary.Get
import Data.Binary

newtype Be a = Be { fromBe :: a } deriving Eq
newtype Le a = Le { fromLe :: a } deriving Eq

instance Binary (Be Word16) where
    put = putWord16be . fromBe
    get = Be <$> getWord16be

instance Binary (Le Word16) where
    put = putWord16le . fromLe
    get = Le <$> getWord16le

instance Binary (Be Word32) where
    put = putWord32be . fromBe
    get = Be <$> getWord32be

instance Binary (Le Word32) where
    put = putWord32le . fromLe
    get = Le <$> getWord32le

instance Binary (Be Word64) where
    put = putWord64be . fromBe
    get = Be <$> getWord64be

instance Binary (Le Word64) where
    put = putWord64le . fromLe
    get = Le <$> getWord64le
