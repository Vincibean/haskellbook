module Ch09.Ch09 where

import           Data.Char

-- Safe head
mySafeHead :: [a] -> Maybe a
mySafeHead []    = Nothing
mySafeHead (x:_) = Just x

-- EnumFromTo

eftBool :: Bool -> Bool -> [Bool]
eftBool = eft

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = eft

eftInt :: Int -> Int -> [Int]
eftInt = eft

eftChar :: Char -> Char -> [Char]
eftChar = eft

eft :: (Ord t, Enum t) => t -> t -> [t]
eft s e
  | s > e = []
  | s == e = [s]
  | otherwise = s : eft (succ s) e

-- Thy Fearful Symmetry
-- 1.

myWords :: String -> [String]
myWords "" = []
myWords xs = word : myWords rest
  where (word, rest) = (takeWhile (/= ' ') words, dropWhile (/= ' ') words) -- better yet: use span
        words = dropWhile (== ' ') xs

myWords' :: String -> [String]
myWords' "" = []
myWords' xs = word : myWords' rest
  where (word, rest) = span (/= ' ') $ dropWhile (== ' ') xs

myWords'' :: String -> [String]
myWords'' "" = []
myWords'' xs = word : myWords'' rest
  where (word, rest) = span (not . isSpace) $ dropWhile (isSpace) xs

-- 3.
-- Basically a synonym of https://hackage.haskell.org/package/bytestring-0.10.10.1/docs/Data-ByteString-Char8.html#v:split
mySplit :: Char -> String -> [String]
mySplit _ "" = []
mySplit c xs = word : mySplit c rest
  where (word, rest) = (takeWhile (/= c) words, dropWhile (/= c) words) -- better yet: use span
        words = dropWhile (== c) xs

myWords''' :: String -> [String]
myWords''' = mySplit ' '

-- Comprehend Thy Lists

-- let mySqr = [x^2 | x <- [1..5]]
--             [1, 4, 9, 16, 25]

-- [x | x <- mySqr, rem x 2 == 0]
--   takes all the even numbers contained in mySqr
--   [4, 16]

-- [(x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50]
--   takes all the numbers contained in mySqr and creates a tuple whose first element will always be less than 50
--   and whose second element will always be greater than 50.
--   However, there's no number in mySqr which is greater than 50, so no value will be generated, resulting in an empty list.
--   []

-- take 5 [ (x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50 ]
--   same as above, but now we only take the first 5 elements. Since take [] simply returns [], this function will simply return
--   []

-- Square Cube

mySqr = [x^2 | x <- [1..5]]

myCube = [y^3 | y <- [1..5]]

-- 1.
mySqrCube = [(x, y) | x <- mySqr, y <- myCube]

-- 2.
mySqrCube' = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]

-- 3.
mySqrCube'' = length [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]

-- Bottom Madness
-- Will it blow up?

-- 1. Will the following expression return a value or be ⊥?
--    [x^y | x <- [1..5], y <- [2, undefined]]
--    ⊥

-- 2. take 1 $ [x^y | x <- [1..5], y <- [2, undefined]]
--    No; it will return the first element: 1^2 -> 1

-- 3. sum [1, undefined, 3]
--    ⊥

-- 4. length [1, 2, undefined]
--    Only the spine is needed, not the actual values, so this will work and return 3

-- 5. length $ [1, 2, 3] ++ undefined
--    length will throw an error on a bottom value if part of the spine itself is bottom; so this will return
--    ⊥

-- 6. take 1 $ filter even [1, 2, 3, undefined]
--    Haskell is lazy and values are calculated on demand; here, we get a result before having to evaluate undefined,
--    so we will get a result: [2]

-- 7. take 1 $ filter even [1, 3, undefined]
--    Haskell is lazy and values are calculated on demand; here, we don't get a result before having to evaluate undefined; so this will return
--    ⊥

-- 8. take 1 $ filter odd [1, 3, undefined]
--    Haskell is lazy and values are calculated on demand; here, we get a result before having to evaluate undefined,
--    so we will get a result: [1]

-- 9. take 2 $ filter odd [1, 3, undefined]
--    Haskell is lazy and values are calculated on demand; here, we get a result before having to evaluate undefined,
--    so we will get a result: [1, 3]

-- 10. take 3 $ filter odd [1, 3, undefined]
--     Haskell is lazy and values are calculated on demand; here, we don't get a result before having to evaluate undefined, so we will get a result:
--     ⊥


-- Is it in normal form?

-- 1. [1, 2, 3, 4, 5]
--    Normal Form and Weak Head Normal Form

-- 2. 1 : 2 : 3 : 4 : _
--    Weak Head Normal Form

-- 3. enumFromTo 1 10
--    Neither

-- 4. length [1, 2, 3, 4, 5]
--    Neither (although [1, 2, 3, 4, 5] is in normal form)

-- 5. sum (enumFromTo 1 10)
--    Neither

-- 6. ['a'..'m'] ++ ['n'..'z']
--    Neither

-- 7. (_, 'b')
--    Weak Head Normal Form

-- More Bottoms
-- 1. take 1 $ map (+1) [undefined, 2, 3]
--    Haskell is lazy and values are calculated on demand; here, we don't get a result before having to evaluate undefined; so this will return
--    ⊥

-- 2. take 1 $ map (+1) [1, undefined, 3]
--    Haskell is lazy and values are calculated on demand; here, we get a result before having to evaluate undefined,
--    so we will get a result: [2]


-- 3. take 2 $ map (+1) [1, undefined, 3]
--    Haskell is lazy and values are calculated on demand; here, we don't get a result before having to evaluate undefined; so this will return
--    ⊥


-- 4. What does the following mystery function do? What is its type?
--    itIsMystery xs = map (\x -> elem x "aeiou") xs
--    It returns a list of Booleans, where each Boolean indicates if a particular character of the input value is a (lowercase) vowel
--    isIsMystery :: [Char] -> [Boolean]

-- 5. What will be the result of the following functions:
-- a) map (^2) [1..10]
--    [1, 4, 9, 16, 25, 36, 49, 64, 81, 100]

-- b) map minimum [[1..10], [10..20], [20..30]]
--    [1, 10, 20]

-- c) map sum [[1..5], [1..5], [1..5]]
--    [15, 15, 15]

-- 6.
-- map (\x -> if x == 3 then (-x) else (x)) [1..10]
-- map (\x -> bool x (-x) (x == 3)) [1..10]

-- Filtering
-- 1. Given the above, how might we write a filter function that would give us all the multiples of 3 out of a list from 1-30?
multiplesOf3 = filter (\x -> x `rem` 3 == 0) [1..30]

-- 2. How could we compose the above function with the length function to tell us *how many* multiples of 3 there are between 1 and 30?
-- length $ filter (\x -> x `rem` 3 == 0) [1..30]

--3. Remove all articles (’the’, ’a’, and ’an’) from sentences
myFilter :: String -> [String]
myFilter = filter (not . flip elem ["the", "a", "an"]) . words

-- Zipping exercises
-- 1. Write your own version of zip :: [a] -> [b] -> [(a, b)] and ensure it behaves the same as the original.
zip' :: [a] -> [b] -> [(a, b)]
zip' [] _          = []
zip' _ []          = []
zip' (a:as) (b:bs) = (a, b) : zip' as bs

-- 2. Do what you did for zip, but now for zipWith :: (a -> b -> c)-> [a] -> [b] -> [c]
zipWith' :: (a -> b -> c)-> [a] -> [b] -> [c]
zipWith' _ [] _          = []
zipWith' _ _ []          = []
zipWith' f (a:as) (b:bs) = f a b : zipWith' f as bs

-- 3. Rewrite your zip in terms of the zipWith you wrote.
zip'' :: [a] -> [b] -> [(a, b)]
zip'' = zipWith' (,)

-- Chapter Exercises

-- Data.Char
-- 1. Query the types of isUpper and toUpper.
-- isUpper :: Char -> Bool
-- toUpper :: Char -> Char

-- 2. Write a function that filters all the uppercase letters out of a String
allUpper :: String -> String
allUpper = filter isUpper

-- 3. Write a function that will capitalize the first letter of a String and return the entire String.
capitalize :: String -> String
capitalize ""     = ""
capitalize (a:as) = toUpper a : as

-- 4. Now make a new version of that function that is recursive such that if you give it the input “woot” it will holler back at you “WOOT.”
holler :: String -> String
holler = map toUpper

holler' :: String -> String
holler' ""     = ""
holler' (a:as) = toUpper a : holler' as

-- 5. Now write a function that will capitalize the first letter of a String and return only that letter as the result.
capitalFirstLetter :: String -> Char
capitalFirstLetter xs = toUpper (head xs)

-- 6. Rewrite it as a composed function. Then, rewrite it pointfree.
capitalFirstLetter' :: String -> Char
capitalFirstLetter' xs = toUpper . head $ xs

capitalFirstLetter'' :: String -> Char
capitalFirstLetter'' = toUpper . head

-- Writing your own standard functions

myAnd :: [Bool] -> Bool
myAnd []     = True
myAnd (x:xs) = if x == False then False else myAnd xs

-- 1. myOr returns True if any Bool in the list is True.
myOr :: [Bool] -> Bool
myOr []     = False
myOr (x:xs) = x || myOr xs

-- 2. myAny returns True if a -> Bool applied to any of the values in the list returns True.
myAny :: (a -> Bool) -> [a] -> Bool
myAny f []     = False
myAny f (x:xs) = f x || myAny f xs

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' f = myOr . map f

-- 3.
myElem :: Eq a => a -> [a] -> Bool
myElem _ []     = False
myElem b (a:as) = b == a || myElem b as

myElem' :: Eq a => a -> [a] -> Bool
myElem' a = any (== a)

-- 4. Implement myReverse.
myReverse :: [a] -> [a]
myReverse []     = []
myReverse (a:as) = myReverse as ++ [a]

-- 5. squish flattens a list of lists into a list
squish :: [[a]] -> [a]
squish []     = []
squish (a:as) = a ++ squish as

-- 6. squishMap maps a function over a list and concatenates the results.
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ []     = []
squishMap f (a:as) = f a ++ squishMap f as

squishMap' :: (a -> [b]) -> [a] -> [b]
squishMap' f = squish . map f

-- 7. squishAgain flattens a list of lists into a list. This time re-use the squishMap function.
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- 8. myMaximumBy takes a comparison function and a list and returns the greatest element of the list based on the last value that the comparison returned GT for
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = error "failing like maximumBy"
myMaximumBy f (a:as) = go f a as
  where go _ a [] = a
        go f max (a:as) = if f a max == GT
                          then go f a as
                          else go f max as

-- 9. myMinimumBy takes a comparison function and a list and returns the least element of the list based on the last value that the comparison returned LT for.
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = error "failing like minimumBy"
myMinimumBy f (a:as) = go f a as
  where go _ a [] = a
        go f max (a:as) = if f a max == LT
                          then go f a as
                          else go f max as


myCompareBy :: Ordering -> (a -> a -> Ordering) -> [a] -> a
myCompareBy _ _ [] = error "failing like Prelude"
myCompareBy ord f (a:as) = go ord f a as
  where go _ _ a [] = a
        go ord f max (a:as) = if f a max == ord
                              then go ord f a as
                              else go ord f max as



myMaximumBy' :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy' = myCompareBy GT


myMinimumBy' :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy' = myCompareBy LT

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy' compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy' compare
