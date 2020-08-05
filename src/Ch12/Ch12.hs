module Ch12.Ch12 where

import Control.Applicative
import qualified Data.Char as C
import Data.List
import qualified Data.Maybe as M

-- Determine the kinds
-- 1. Given
-- id :: a -> a
-- What is the kind of a?
--    a :: *

-- 2. r :: a -> f a
-- What are the kinds of a and f?
--    a :: *
--    f :: * -> *

-- String processing
-- 1. Write a recursive function named replaceThe which takes a text/string,
-- breaks it into words and replaces each instance of "the" with "a".

toLower :: String -> String
toLower = map C.toLower

-- notThe "the" -> Nothing
-- notThe "blahtheblah" -> Just "blahtheblah"
-- notThe "woot" -> Just "woot"
notThe :: String -> Maybe String
notThe txt = case toLower txt of
  "the" -> Nothing
  _ -> Just txt

-- replaceThe "the cow loves us" -> "a cow loves us"
replaceThe :: String -> String
replaceThe = unwords . map (replaceWithA . notThe) . words

replaceWithA :: Maybe String -> String
replaceWithA = M.fromMaybe "a"

-- 2. Write a recursive function that takes a text/string, breaks it into words, and counts the number of instances of "the" followed by a vowel-initial word.

-- countTheBeforeVowel "the cow" -> 0
-- countTheBeforeVowel "the evil cow" -> 1
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel s = toInteger . length . filter (uncurry p) $ coupleWords
  where
    allWords = words s
    coupleWords = allWords `zip` tail allWords
    p w1 w2 = isThe w1 && startsWithVowel w2

startsWithVowel :: String -> Bool
startsWithVowel = any (`elem` vowels) . M.listToMaybe
  where
    vowels = "aeiouyAEIOUY"

isThe :: String -> Bool
isThe = M.isNothing . notThe

-- 3. Return the number of letters that are vowels in a word.

-- countVowels "the cow" -> 2
-- countVowels "Mikolajczak" -> 4
countVowels :: String -> Integer
countVowels = toInteger . length . filter isVowel

isVowel :: Char -> Bool
isVowel = flip elem "aeiouyAEIOUY"

-- Validate the word
newtype Word' = Word' String deriving (Eq, Show)

vowels = "aeiou" ++ "AEIOU"

consonants = (['a' .. 'z'] ++ ['A' .. 'Z']) \\ vowels

isConsonant :: Char -> Bool
isConsonant = flip elem consonants

countConsonants :: String -> Integer
countConsonants = toInteger . length . filter isConsonant

mkWord :: String -> Maybe Word'
mkWord s =
  if countVowels s > countConsonants s
    then Nothing
    else Just $ Word' s

-- It’s only Natural

-- As natural as any competitive bodybuilder
data Nat = Zero | Succ Nat deriving (Eq, Show)

-- natToInteger Zero -> 0
-- natToInteger (Succ Zero) -> 1
-- natToInteger (Succ (Succ Zero)) -> 2
natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ nat) = 1 + natToInteger nat

-- integerToNat 0 -> Just Zero
-- integerToNat 1 -> Just (Succ Zero)
-- integerToNat 2 -> Just (Succ (Succ Zero))
-- integerToNat (-1) -> Nothing
integerToNat :: Integer -> Maybe Nat
integerToNat i
  | i < 0 = Nothing
  | otherwise = Just $ go i
  where
    go 0 = Zero
    go x = Succ . go $ x - 1

-- Small library for Maybe

-- 1. Simple boolean checks for Maybe values.
-- isJust (Just 1) -> True
-- isJust Nothing -> False
isJust :: Maybe a -> Bool
isJust = not . isNothing

-- isNothing (Just 1) -> False
-- isNothing Nothing -> True
isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

-- 2. The following is the Maybe catamorphism. You can turn a Maybe value into anything else with this.

-- mayybee 0 (+1) Nothing -> 0
-- mayybee 0 (+1) (Just 1) -> 2
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing = b
mayybee b f (Just a) = f a

-- 3. In case you just want to provide a fallback value.

-- fromMaybe 0 Nothing -> 0
-- fromMaybe 0 (Just 1) -> 1
fromMaybe :: a -> Maybe a -> a
fromMaybe a = mayybee a id

-- 4. Converting between List and Maybe.

-- listToMaybe [1, 2, 3] -> Just 1
-- listToMaybe [] -> Nothing
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (a : _) = Just a

-- maybeToList (Just 1) -> [1]
-- maybeToList Nothing -> []
maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

maybeToList' :: Maybe a -> [a]
maybeToList' = mayybee [] (: [])

-- 5. For when we just want to drop the Nothing values from our list.

-- catMaybes [Just 1, Nothing, Just 2] -> [1, 2]
-- catMaybes [Nothing, Nothing, Nothing] -> []
catMaybes :: [Maybe a] -> [a]
catMaybes = concatMap maybeToList

-- 6. You’ll see this called "sequence" later.

-- flipMaybe [Just 1, Just 2, Just 3] -> Just [1, 2, 3]
-- flipMaybe [Just 1, Nothing, Just 3] -> Nothing
-- flipMaybe ([] :: [Maybe Int]) -> Just []
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr f (Just [])
  where
    f (Just x) (Just xs) = Just $ x : xs
    f _ _ = Nothing

flipMaybe' :: [Maybe a] -> Maybe [a]
flipMaybe' = foldr (liftA2 (:)) (Just [])

-- Small library for Either

-- 1. Try to eventually arrive at a solution that uses foldr, even if earlier versions don’t use foldr.
lefts' :: [Either a b] -> [a]
lefts' = foldr f []
  where
    f (Left a) as = a : as
    f _ as = as

-- 2. Same as the last one. Use foldr eventually
rights' :: [Either a b] -> [b]
rights' = foldr f []
  where
    f (Right b) bs = b : bs
    f _ bs = bs

-- 3.
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' es = (lefts' es, rights' es)

-- 4.
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right b) = Just $ f b

-- 5. This is a general catamorphism for Either values.
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' lf _ (Left a) = lf a
either' _ rf (Right b) = rf b

-- 6. Same as before, but use the either' function you just wrote.
eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)

-- Unfolds
-- Write your own iterate and unfoldr
-- 1. Write the function myIterate using direct recursion.
myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

-- 2. Write the function myUnfoldr using direct recursion.
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = case f b of
  Nothing -> []
  (Just (a, b')) -> a : myUnfoldr f b'

-- 3. Rewrite myIterate into betterIterate using myUnfoldr.
betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\a -> Just (a, f a))

-- Finally something other than a list!

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

-- 1. Write unfold for BinaryTree
unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f a = case f a of
  Nothing -> Leaf
  (Just (la, b, ra)) -> Node (unfold f la) b (unfold f ra)

-- 2. Make a tree builder.
treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold f 0
  where
    f i =
      if i >= n
        then Nothing
        else Just (i + 1, i, i + 1)
