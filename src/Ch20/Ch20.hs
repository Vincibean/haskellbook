module Ch20.Ch20 where

import           Data.Monoid
import           Data.Semigroup
import           Data.Bool                      ( bool )

{-# ANN fold' "HLint: ignore" #-}
{-# ANN sum'' "HLint: ignore" #-}
{-# ANN product'' "HLint: ignore" #-}

-- Exercises: Library Functions

-- 1. This and the next one are nicer with foldMap, but foldr is fine too.
sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

sum'' :: (Foldable t, Num a) => t a -> a
sum'' = foldr (+) 0

-- 2. 
product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

product'' :: (Foldable t, Num a) => t a -> a
product'' = foldr (*) 1

-- 3. 
elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' el = foldr (\x acc -> x == el || acc) False

elem'' :: (Foldable t, Eq a) => a -> t a -> Bool
elem'' el = getAny . foldMap (Any . (== el))

-- 4. 
minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' xs = getMin <$> foldr ((<>) . Just . Min) Nothing xs

-- 5. 
maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' xs = getMax <$> foldr ((<>) . Just . Max) Nothing xs

-- 6. 
null' :: (Foldable t) => t a -> Bool
null' = foldr (\_ _ -> False) True

-- 7. 
length' :: (Foldable t) => t a -> Int
length' = getSum . foldMap (const $ Sum 1)

-- 8. Some say this is all Foldable amounts to.
toList' :: (Foldable t) => t a -> [a]
toList' = foldMap (: [])

-- 9. Hint: use foldMap.
-- | Combine the elements of a structure using a monoid.
fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

-- 10. Define foldMap in terms of foldr.
foldMap'' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap'' f = foldr (\el acc -> f el <> acc) mempty

-- Chapter Exercises
-- Write Foldable instances for the following datatypes.

-- 1. 
newtype Constant a b = Constant a deriving (Eq, Show)

instance Foldable (Constant a) where
  foldr _ x _ = x

newtype Constant' a b = Constant' b deriving (Eq, Show)

instance Foldable (Constant' a) where
  foldMap f (Constant' b) = f b

-- 2. 
data Two a b = Two a b deriving (Eq, Show)

instance Foldable (Two a) where
  foldr f x (Two _ b) = f b x

-- 3. 
data Three a b c = Three a b c deriving (Eq, Show)

instance Foldable (Three a b) where
  foldr f x (Three a b c) = f c x

-- 4. 
data Three' a b = Three' a b b deriving (Eq, Show)

instance Foldable (Three' a) where
  foldMap f (Three' _ b b') = f b <> f b'

-- 5. 
data Four' a b = Four' a b b b deriving (Eq, Show)

instance Foldable (Four' a) where
  foldMap f (Four' a b b' b'') = f b <> f b' <> f b''

-- Thinking cap time. Write a filter function for Foldable types using foldMap.
filterF
  :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF pred = foldMap (\x -> if pred x then pure x else mempty)

filterF'
  :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF' pred = foldMap (\x -> bool mempty (pure x) (pred x))
