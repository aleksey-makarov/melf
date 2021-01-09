-- This differs from intervals package: even an empty interval has offset (but not inf)

module Data.Interval
    ( Interval(..)
    , member
    , empty
    , contains
    ) where

data Interval a = I { offset :: !a, size :: !a } deriving (Eq, Ord)

instance (Ord a, Num a, Show a) => Show (Interval a) where
  show (I o s) | s <= 0    = "empty @" ++ show o
  show (I o s) | otherwise = show o ++ " ... " ++ show (o + s - 1)

member :: (Ord a, Num a) => a -> Interval a -> Bool
member _ (I _ s) | s <= 0    = False
member x (I o s) | otherwise = o <= x && x <= (o + s - 1)
{-# INLINE member #-}

empty :: (Ord a, Num a) => Interval a -> Bool
empty (I _ s) | s <= 0    = True
empty _       | otherwise = False
{-# INLINE empty #-}

contains :: (Ord a, Num a) => Interval a -> Interval a -> Bool
contains a (I o s) | s <= 0    = o `member` a
contains a b       | otherwise = offset b `member` a && (offset b + size b - 1) `member` a
{-# INLINE contains #-}
