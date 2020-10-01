module Ch21.Ch21 where

import           Control.Applicative            ( liftA3 )

-- Traversable instances

-- 1. Identity
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Foldable Identity where
  foldMap f (Identity a) = f a

instance Traversable Identity where
  traverse f (Identity x) = Identity <$> f x

-- 2. Constant
newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
  foldr _ x _ = x

instance Traversable (Constant a) where
  traverse f (Constant a) = pure $ Constant a

-- 3. Maybe
data Optional a = Nada | Yep a deriving (Eq, Show)

instance Functor Optional where
  fmap _ Nada    = Nada
  fmap f (Yep a) = Yep $ f a

instance Foldable Optional where
  foldr _ x Nada    = x
  foldr f x (Yep a) = f a x

instance Traversable Optional where
  traverse _ Nada    = pure Nada
  traverse f (Yep a) = Yep <$> f a

-- 4. List
data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Foldable List where
  foldMap _ Nil         = mempty
  foldMap f (Cons a as) = f a <> foldMap f as

instance Traversable List where
  traverse f Nil         = pure Nil
  traverse f (Cons a as) = Cons <$> f a <*> traverse f as

-- 5. Three
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b $ f c

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c

-- 6. Pair
data Pair a b = Pair a b deriving (Eq, Show)

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a $ f b

instance Foldable (Pair a) where
  foldMap f (Pair a b) = f b

instance Traversable (Pair a) where
  traverse f (Pair a b) = Pair a <$> f b

-- 7. Big
data Big a b = Big a b b deriving (Eq, Show)

instance Functor (Big a) where
  fmap f (Big a b b') = Big a (f b) (f b')

instance Foldable (Big a) where
  foldMap f (Big a b b') = f b <> f b'

instance Traversable (Big a) where
  traverse f (Big a b b') = Big a <$> f b <*> f b'

-- 8. Bigger
data Bigger a b = Bigger a b b b deriving (Eq, Show)

instance Functor (Bigger a) where
  fmap f (Bigger a b b' b'') = Bigger a (f b) (f b') (f b'')

instance Foldable (Bigger a) where
  foldMap f (Bigger a b b' b'') = f b <> f b' <> f b''

instance Traversable (Bigger a) where
  traverse f (Bigger a b b' b'') = liftA3 (Bigger a) (f b) (f b') (f b'')

-- 9. S
data S n a = S (n a) a deriving (Eq, Show)

instance (Functor n) => Functor (S n) where
  fmap f (S na a) = S (f <$> na) (f a)

instance (Foldable n) => Foldable (S n) where
  foldMap f (S na a) = foldMap f na <> f a

instance Traversable n => Traversable (S n) where
  traverse f (S na a) = S <$> traverse f na <*> f a

-- 10. Instances for Tree
data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty          = Empty
  fmap f (Leaf a      ) = Leaf $ f a
  fmap f (Node ls x rs) = Node (fmap f ls) (f x) (fmap f rs)

-- foldMap is a bit easier and looks more natural,
-- but you can do foldr too for extra credit.
instance Foldable Tree where
  foldr _ x Empty    = x
  foldr f x (Leaf a) = f a x
  foldr f x (Node ls a rs) =
    let acc1 = f a x
        acc2 = foldr f acc1 rs
    in  foldr f acc2 ls

  foldMap _ Empty          = mempty
  foldMap f (Leaf a      ) = f a
  foldMap f (Node ls x rs) = foldMap f ls <> f x <> foldMap f rs


instance Traversable Tree where
  traverse f Empty          = pure Empty
  traverse f (Leaf a      ) = Leaf <$> f a
  traverse f (Node ls x rs) = Node <$> traverse f ls <*> f x <*> traverse f rs
