module Ch17.Ch17 where

import           Data.List                      ( elemIndex )

import           Control.Applicative            ( liftA3 )

-- Lookups

-- 1.
added :: Maybe Integer
added = (+ 3) <$> lookup 3 (zip [1, 2, 3] [4, 5, 6])

-- 2.
y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

-- 3.

x :: Maybe Int
x = elemIndex 3 [1, 2, 3, 4, 5]

y' :: Maybe Int
y' = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x <*> y'

-- 4. 

xs = [1, 2, 3]

ys = [4, 5, 6]

x' :: Maybe Integer
x' = lookup 3 $ zip xs ys

y'' :: Maybe Integer
y'' = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = pure sum <*> ((,) <$> x' <*> y'')

-- Identity Instance
-- Write an Applicative instance for Identity.
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity

  (Identity f) <*> (Identity a) = Identity $ f a
  -- Alternatively
  -- (Identity f) <*> x = f <$> x

-- Constant Instance

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure = Constant . mempty

  (Constant a) <*> (Constant a') = Constant $ a <> a'

--  Fixer Upper

-- 1. 
fixreUpper1 = const <$> Just "Hello" <*> pure "World"

-- 2. 
fixreUpper2 =
  (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]

-- List Applicative Exercise
data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil           = Nil
  fmap f (Cons a list) = Cons (f a) (f <$> list)

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

-- write this one in terms of concat' and fmap
flatMap :: (a -> List b) -> List a -> List b
flatMap f = concat' . fmap f

-- ZipList Applicative Exercise

take' :: Int -> List a -> List a
take' i _ | i <= 0  = Nil
take' i Nil         = Nil
take' i (Cons a as) = Cons a (take' (i - 1) as)

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure a = ZipList' $ Cons a (pure a)

  (ZipList' Nil) <*> (ZipList' Nil) = ZipList' Nil
  (ZipList' Nil) <*> (ZipList' _  ) = ZipList' Nil
  (ZipList' _  ) <*> (ZipList' Nil) = ZipList' Nil
  (ZipList' (Cons f fs)) <*> (ZipList' (Cons a as)) =
    ZipList' $ Cons (f a) (fs <*> as)

-- Variations on Either

data Validation e a = Failure e | Success a deriving (Eq, Show)

instance Functor (Validation e) where
  fmap f (Failure e) = Failure e
  fmap f (Success a) = Success $ f a

instance Monoid e => Applicative (Validation e) where
  pure = Success

  (Failure e) <*> (Failure e') = Failure $ e <> e'
  (Failure e) <*> (Success a ) = Failure e
  (Success f) <*> (Failure e ) = Failure e
  (Success f) <*> (Success a') = Success $ f a'

-- Chapter Exercises

-- Specialize the types of the methods. 

-- 1. Type [] 
--    pure :: a -> [a]
--    (<*>) :: [(a -> b)] -> [a] -> [b]

-- 2. Type IO
--    pure :: a -> IO a
--    (<*>) :: IO (a -> b) -> IO a -> IO b

-- 3. Type (,) x
--    pure :: a -> (x, a)
--    (<*>) :: (x, (a -> b)) -> (x, a) -> (x, b)

-- 4. Type (->) e
--    pure :: a -> (e -> a)
--    (<*>) :: (e -> (a -> b)) -> (e -> a) -> (e -> b)

-- Write applicative instances for the following datatypes. 
-- 1. 
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
  pure a = Pair a a

  (Pair f f') <*> (Pair a a') = Pair (f a) (f' a')

-- 2.
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a $ f b

instance (Monoid a) => Applicative (Two a) where
  pure = Two mempty

  (Two a f) <*> (Two a' a'') = Two (a <> a') $ f a''

-- 3.
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b $ f c

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty

  (Three a b f) <*> (Three a' b' c) = Three (a <> a') (b <> b') $ f c

-- 4. 
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance (Monoid a) => Applicative (Three' a) where
  pure b = Three' mempty b b

  (Three' a f f') <*> (Three' a' b b') = Three' (a <> a') (f b) $ f' b'

-- 5. 
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c $ f d

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure = Four mempty mempty mempty

  (Four a b c f) <*> (Four a' b' c' d) =
    Four (a <> a') (b <> b') (c <> c') $ f d

-- 6.
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a a' a'' b) = Four' a a' a'' $ f b

instance (Monoid a) => Applicative (Four' a) where
  pure = Four' mempty mempty mempty

  (Four' la la' la'' f) <*> (Four' ra ra' ra'' b) =
    Four' (la <> ra) (la' <> ra') (la'' <> ra'') $ f b

-- Combinations
stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)
