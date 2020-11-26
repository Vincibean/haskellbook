{-# LANGUAGE Strict #-}

module Ch27.StrictList where

data List a = Nil | Cons a (List a) deriving (Show)
-- We can also reintroduce lazyness with the usage of (~); e.g.:
-- data List a = Nil | Cons a ~(List a) deriving (Show)
-- see: https://en.wikibooks.org/wiki/Haskell/Laziness#Lazy_pattern_matching

take' n _ | n <= 0 = Nil
take' _ Nil = Nil
take' n (Cons x xs) = (Cons x (take' (n-1) xs))

map' _ Nil = Nil
map' f (Cons x xs) = (Cons (f x) (map' f xs))

repeat' x = xs where xs = (Cons x xs)
main = do
    print $ take' 10 $ map' (+1) (repeat' 1)