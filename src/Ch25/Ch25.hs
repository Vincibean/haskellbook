{-# LANGUAGE InstanceSigs #-}

module Ch25.Ch25 where

import           Control.Applicative            ( Applicative(liftA2) )
import           Data.Bifunctor                 ( Bifunctor(bimap) )

newtype Compose f g a = Compose { getCompose :: f (g a) } deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

-- GOTCHA! Exercise time

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure = Compose . pure . pure

  -- https://github.com/BoeingX/haskell-programming-from-first-principles/blob/ffb637f536597f552a4e4567fee848ed27f3ba74/src/ComposingTypes/Twinplicative/Exercise.hs#L13
  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (Compose fgab) <*> (Compose fga) = Compose $ liftA2 (<*>) fgab fga


-- Compose instances
-- 1. Write the Compose Foldable instance.
instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap f (Compose fga) = (foldMap . foldMap) f fga

-- 2. Write the Compose Traversable instance:
instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse
    :: Applicative f1 => (a -> f1 b) -> Compose f g a -> f1 (Compose f g b)
  traverse f (Compose fga) = Compose <$> (traverse . traverse) f fga


-- And now for something completely different
-- 1. 
data Deux a b = Deux a b deriving (Eq, Show)

instance Bifunctor Deux where
  bimap lf rf (Deux a b) = Deux (lf a) (rf b)

-- 2. 
newtype Const a b = Const a deriving (Eq, Show)

instance Bifunctor Const where
  bimap lf _ (Const a) = Const (lf a)

-- 3. 
data Drei a b c = Drei a b c deriving (Eq, Show)

instance Bifunctor (Drei a) where
  bimap lf rf (Drei a b c) = Drei a (lf b) (rf c)

-- 4. 
data SuperDrei a b c = SuperDrei a b deriving (Eq, Show)

instance Bifunctor (SuperDrei a) where
  bimap lf _ (SuperDrei a b) = SuperDrei a (lf b)

newtype SuperDrei' c a b = SuperDrei' { getSuperDrei :: SuperDrei a b c } deriving (Eq, Show)

instance Bifunctor (SuperDrei' c) where
  bimap lf rf (SuperDrei' (SuperDrei a b)) =
    SuperDrei' $ SuperDrei (lf a) (rf b)

-- 5. 
newtype SemiDrei a b c = SemiDrei a deriving (Eq, Show)

instance Bifunctor (SemiDrei a) where
  bimap _ _ (SemiDrei a) = SemiDrei a

-- 6.
data Quadriceps a b c d = Quadzzz a b c d deriving (Eq, Show)

instance Bifunctor (Quadriceps a b) where
  bimap lf rf (Quadzzz a b c d) = Quadzzz a b (lf c) (rf d)

-- 7. 
data Either' a b = Left' a | Right' b deriving (Eq, Show)

instance Bifunctor Either' where
  bimap lf _  (Left'  a) = Left' $ lf a
  bimap _  rf (Right' b) = Right' $ rf b
