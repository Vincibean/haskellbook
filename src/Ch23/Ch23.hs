{-# LANGUAGE TupleSections #-}
{-# LANGUAGE InstanceSigs #-}

module Ch23.Ch23 where

import System.Random ( Random(randomR), StdGen )

import Control.Monad.Trans.State ( execState, get, put, State )

import qualified Data.DList                    as DL


-- Six-sided die
data Die = DieOne | DieTwo | DieThree | DieFour | DieFive | DieSix deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n = case n of
  1 -> DieOne
  2 -> DieTwo
  3 -> DieThree
  4 -> DieFour
  5 -> DieFive
  6 -> DieSix
  x -> error $ "intToDie got non 1-6 integer: " ++ show x

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty = go 0 0
 where
  go :: Int -> Int -> StdGen -> Int
  go sum count gen
    | sum >= 20
    = count
    | otherwise
    = let (die, nextGen) = randomR (1, 6) gen
      in  go (sum + die) (count + 1) nextGen

-- Roll your own
-- 1. Refactor rollsToGetTwenty so that the limit is an argument to the function:

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n = go 0 0
 where
  go :: Int -> Int -> StdGen -> Int
  go sum count gen
    | sum >= n
    = count
    | otherwise
    = let (die, nextGen) = randomR (1, 6) gen
      in  go (sum + die) (count + 1) nextGen

-- 2. Change rollsToGetN to record the series of dice that are rolled, in addition to the count of the total number of rolls:

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n g = (i, list)
 where
  list       = DL.apply dlist []
  (i, dlist) = go 0 DL.empty 0 g

  go :: Int -> DL.DList Die -> Int -> StdGen -> (Int, DL.DList Die)
  go sum rolls count gen
    | sum >= n
    = (count, rolls)
    | otherwise
    = let (i, nextGen) = randomR (1, 6) gen
          die          = intToDie i
      in  go (sum + i) (DL.snoc rolls die) (count + 1) nextGen

-- Write State for yourself

newtype Moi s a = Moi { runMoi :: s -> (a, s) }

-- State Functor
-- Implement the Functor instance for State

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ \s -> let (a, s') = g s in (f a, s')
-- fmap a2b (Moi s2as) = Moi $ (\(a, s) -> (a2b a, s)) . s2as


-- State Applicative
-- Write the Applicative instance for State

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi (a, )

  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (Moi f) <*> (Moi g) = Moi $ \s ->
    let (f', s' ) = f s
        (a , s'') = g s'
    in  (f' a, s'')

-- State Monad
-- Write the Monad instance for State

instance Monad (Moi s) where
  return = pure

  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (Moi f) >>= g = Moi $ \s ->
    let (a, s') = f s
        moisb   = g a
        res     = runMoi moisb s'
    in  res

-- FizzBuzz differently
fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5 == 0  = "Buzz"
           | n `mod` 3 == 0  = "Fizz"
           | otherwise       = show n

fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list = execState (mapM_ addResult list) []

addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put $ result : xs

fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo from to = fizzbuzzList [to, (to - 1) .. from]

-- Chapter exercises

-- 1. Construct a State where the state is also the value you return
get' :: Moi s s
get' = Moi $ \s -> (s, s)

-- 2. Construct a State where the resulting state is the argument provided, and the value defaults to unit
put' :: s -> Moi s ()
put' s = Moi $ const ((), s)

-- 3. Run the State with s and get the state that results
exec' :: Moi s a -> s -> s
exec' (Moi sa) = snd . sa

-- 4. Run the State with s and get the value that results
eval' :: Moi s a -> s -> a
eval' (Moi sa) = fst . sa

-- 5. Write a function that applies a function to create a new State:
modify' :: (s -> s) -> Moi s ()
modify' f = do
  s <- get'
  put' $ f s

modify'' :: (s -> s) -> Moi s ()
modify'' f = get' >>= put' . f

modify''' :: (s -> s) -> Moi s ()
modify''' f = Moi $ \s -> ((), f s)
