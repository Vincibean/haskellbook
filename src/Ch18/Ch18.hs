module Ch18.Ch18 where

import           Control.Monad                  ( join )
import           Control.Applicative

{-# ANN bind "HLint: ignore" #-}

-- The answer is the exercise
-- Write bind in terms of fmap and join.

-- keep in mind this is (>>=) flipped
bind :: Monad m => (a -> m b) -> m a -> m b
bind f = join . fmap f

-- Either Monad
-- Implement the Either Monad.

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (First  a) = First a
  fmap f (Second b) = Second $ f b

instance Applicative (Sum a) where
  pure = Second

  (First  a) <*> _   = First a
  (Second f) <*> sum = f <$> sum

instance Monad (Sum a) where
  return = pure

  (First  a) >>= f = First a
  (Second b) >>= f = f b

-- Chapter Exercises
-- 1.
data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg

  _ <*> _ = NopeDotJpg

instance Monad Nope where
  _ >>= _ = NopeDotJpg

-- 2. 
data PhhhbbtttEither b a = Left' a | Right' b deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
  fmap f (Right' b) = Right' b
  fmap f (Left'  a) = Left' $ f a

instance Applicative (PhhhbbtttEither b) where
  pure = Left'

  (Right' b) <*> _ = Right' b
  (Left'  f) <*> e = f <$> e

instance Monad (PhhhbbtttEither b) where
  (Right' b) >>= _ = Right' b
  (Left'  a) >>= f = f a

-- 3.
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity

  (Identity f) <*> (Identity a) = Identity $ f a

instance Monad Identity where
  (Identity a) >>= f = f a

-- 4. 
data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap f Nil         = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Applicative List where
  pure a = Cons a Nil

  Nil <*> _   = Nil
  _   <*> Nil = Nil
  c1  <*> c2  = flatMap (<$> c2) c1

append :: List a -> List a -> List a
append Nil         ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil        = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f = concat' . fmap f

instance Monad List where
  (>>=) = flip flatMap

-- instance Monad List where
--   Nil >>= _ = Nil
--   (Cons a as) >>= f = f a `append` (as >>= f)  

-- Write the following functions using the methods provided by Monad and Functor. 
-- 1. 
j :: Monad m => m (m a) -> m a
j = join

-- 2. 
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

-- 3. 
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftA2

l2' :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2' f ma mb = do
  a <- ma
  f a <$> mb

-- 4. 
a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

a' :: Monad m => m a -> m (a -> b) -> m b
a' ma mf = do
  f <- mf
  f <$> ma

-- 5.
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh []       _ = pure []
meh (a : as) f = do
  b  <- f a
  bs <- meh as f
  return $ b : bs

-- 6.
flipType :: (Monad m) => [m a] -> m [a]
flipType mas = meh mas id
