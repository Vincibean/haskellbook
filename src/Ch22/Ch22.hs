{-# LANGUAGE InstanceSigs #-}

module Ch22.Ch22 where

import           Data.Char

import Control.Applicative
import Data.Maybe

import Data.Monoid

-- Warming up

cap :: String -> String
cap = map toUpper

rev :: String -> String
rev = reverse

composed :: String -> String
composed = cap . rev

fmapped :: String -> String
fmapped = fmap cap rev

fmapped' :: String -> String
fmapped' = cap <$> rev

tupled :: String -> (String, String)
tupled = do
  a <- cap
  b <- rev
  return (a, b)

tupled' :: String -> (String, String)
tupled' = rev >>= (\a -> cap >>= (\b -> return (a, b)))

tupled'' :: String -> (String, String)
tupled'' = (,) <$> cap <*> rev

-- Ask

newtype Reader r a = Reader { runReader :: r -> a }

ask :: Reader a a
ask = Reader id

-- Reading comprehension
-- 1. Write liftA2 yourself. Think about it in terms of abstracting out the difference between getDogR and getDogR', if that helps.

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f c1 c2 = f <$> c1 <*> c2

-- 2. Write the following function. Again, it is simpler than it looks.
asks :: (r -> a) -> Reader r a
asks = Reader

-- 3. Implement the Applicative for Reader.

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader ra) = Reader $ f . ra

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ const a

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader rab) <*> (Reader ra) = Reader $ \r -> let ab = rab r in fmap ab ra r
--  (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r $ ra r
--  (Reader rab) <*> (Reader ra) = Reader $ rab <*> ra

-- Reader Monad
-- 1. Implement the Reader Monad.
instance Monad (Reader r) where
  return = pure

  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  ra >>= aRb = let rab = Reader (flip $ runReader . aRb) in rab <*> ra
  -- (Reader ra) >>= aRb = Reader $ \r ->
  --   let a          = ra r
  --       (Reader f) = aRb a
  --   in  f r

-- 2. Rewrite the monadic getDogRM to use your Reader datatype.

newtype HumanName = HumanName String deriving (Eq, Show)

newtype DogName = DogName String deriving (Eq, Show)

newtype Address = Address String deriving (Eq, Show)

data Person = Person { humanName :: HumanName , dogName :: DogName , address :: Address } deriving (Eq, Show)

data Dog = Dog { dogsName :: DogName , dogsAddress :: Address } deriving (Eq, Show)

pers :: Person
pers = Person (HumanName "Big Bird") (DogName "Barkley") (Address "Sesame Street")

getDogRM :: Reader Person Dog
getDogRM = do
  name <- Reader dogName
  addy <- Reader address
  return $ Dog name addy

getDogRM' :: Reader Person Dog
getDogRM' = Reader $ Dog <$> dogName <*> address

-- Chapter exercises
-- A warm-up stretch

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

-- zip x and y using 3 as the lookup key
xs :: Maybe Integer
xs = lookup 3 $ x `zip` y

-- zip y and z using 6 as the lookup key
ys :: Maybe Integer
ys = lookup 6 $ y `zip` z

-- zip x and y using 4 as the lookup key
zs :: Maybe Integer
zs = lookup 4 $ x `zip` y

-- now zip x and z using a variable lookup key
z' :: Integer -> Maybe Integer
z' n = lookup n $ x `zip` z

-- Have x1 make a tuple of xs and ys
x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

-- Have x2 make a tuple of of ys and zs
x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

-- Write x3, which takes one input and makes a tuple
-- of the results of two applications of z' from above:
x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 = (,) <$> z' <*> z'

x3' :: Integer -> (Maybe Integer, Maybe Integer)
x3' n = let x = z' n in (x, x)

-- summed is uncurry with addition as the first argument
summed :: Num c => (c, c) -> c
summed = uncurry (+)

-- And now, we’ll make a function similar to some we’ve seen before that lifts a Boolean function over two partially applied functions:
-- use &&, >3, <8
bolt :: Integer -> Bool
bolt = (&&) <$> (> 3) <*> (< 8)

sequA :: Integral a => a -> [Bool]
sequA = sequenceA [(> 3), (< 8), even]

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)

-- 1. Fold the Boolean conjunction operator over the list of results of sequA (applied to some value)
wu1 :: Integral a => a -> Bool
wu1 = getAll . foldMap All . sequA

-- 2. Apply sequA to s'
wu2 :: [Bool]
wu2 = maybe mempty sequA s'

-- 3. Apply bolt to ys
wu3 :: Bool
wu3 = maybe False bolt ys