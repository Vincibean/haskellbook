module Ch07.Ch07 where

-- Grab Bag
-- 1. Which (two or more) of the following are equivalent?
--    a) mTh x y z = x * y * z
--    b) mTh x y = \z -> x * y * z
--    c) mTh x = \y -> \z -> x * y * z
--    d) mTh = \x -> \y -> \z -> x * y * z

-- The type of mTh (above) isNum a => a -> a -> a -> a. Which is the type of mTh 3?
--    d) Num a => a -> a -> a

-- 3.
--    a)
addOneIfOdd n = case odd n of
    True  -> f n
    False -> n
    where f = \n -> n + 1
    
--    b)
addFive = \x -> \y -> (if x > y then y else x) + 5

addFive' = \x y -> (if x > y then y else x) + 5

--    c)
mflip f x y = f y x

-- Variety Pack
-- 1. Given the following declarations
k (x, y) = x
k1 = k ((4 - 1), 10)
k2 = k ("three", (1 + 2))
k3 = k (3, True)
--   a) What is the type of k?
--      k :: (x, y) -> x
--   b) What is the type of k2? Is it the same type as k1 or k3?
--      k1 :: Num a => a (or Integer?)
--      k2 :: [Char]
--      k3 :: Num a => a (or Integer?)
--   c) Of k1, k2, k3, which will return the number 3 as the result?
--      k1 and k3

-- 2. Fill in the definition of the following function
f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, b, c) (d, e, f) = ((a, d), (c, f))

-- Case Practice
-- 1.
functionC x y = if (x > y) then x else y

functionC' x y = case x > y of
    True  -> x
    False -> y

-- 2.
ifEvenAdd2 n = if even n then (n + 2) else n

ifEvenAdd2' n = case even n of
    True  -> n + 2
    False -> n

-- 3.
nums x = case compare x 0 of 
    LT -> -1
    GT -> 1
    EQ -> 0

-- Artful Dodgy

dodgy :: (Num a) => a -> a -> a
dodgy x y = x + y * 10 

oneIsOne :: (Num a) => a -> a
oneIsOne = dodgy 1 

oneIsTwo :: (Num a) => a -> a
oneIsTwo = (flip dodgy) 2

-- 2.
-- dodgy 1 1 = 11

-- 3.
-- dodgy 2 2 = 22

-- 4.
-- dodgy 1 2 = 21

-- 5.
-- dodgy 2 1 = 12

-- 6.
-- oneIsOne 1 = 11

-- 7.
-- oneIsOne 2 = 21

-- 8.
-- oneIsTwo 1 = 21

-- 9.
-- oneIsTwo 2 = 22

-- 10.
-- oneIsOne 3 = 31

-- 11.
-- oneIsTwo 3 = 23

-- Guard Duty
-- 2. What happens if you take avgGrade as it is written and reorder the guards? Does it still typecheck and work the same? 
--    It still typechecks; it doesn't work properly anymore
-- Try moving | y >= 0.7 = 'C' and passing it the argument 90, which should be an ‘A.’ Does it return an ‘A’?
--    No

-- 3.
--    b) True when xs is a palindrome

-- 4. What types of arguments can pal take?
--    Eq a => [a]

-- 5. What is the type of the function pal?
--    pal :: Eq a => [a] -> Bool

-- 6. The following function returns
--    c) an indication of whether its argument is a positive or negative number or zero

-- 7. What types of arguments can numbers take?
--    (Ord a, Num a, Num p) => a

-- 8. What is the type of the function numbers?
--    numbers :: (Ord a, Num a, Num p) => a -> p

-- Multiple choice
-- 1. A polymorphic function
--    d) may resolve to values of different types, depending on inputs

-- 2. Two functions named f and g have types Char -> String and String -> [String] respectively. The composed function g . f has the type
--    b) Char -> [String]

-- 3. A function f has the type Ord a => a -> a -> Bool and we apply it to one numeric value. What is the type now?
--    d) (Ord a, Num a) => a -> Bool

-- 4. A function with the type (a -> b) -> c
--    b) is a higher-order function

-- 5. Given the following definition of f, what is the type of f True? 
--    f :: a -> a
--    f x = x
--    a) f True :: Bool

-- Let’s write code
-- 1.a
tensDigit :: Integral a => a -> a
tensDigit x = d 
  where xLast = x `div` 10 
        d     = xLast `mod` 10

tensDigit' :: Integral a => a -> a
tensDigit' x = d 
  where (xLast, _) = x `divMod` 10 
        (_, d)     = xLast `divMod` 10

-- 1.b Does the divMod version have the same type as the original version?
--     Yes

-- 1.c
hunsD :: Integral a => a -> a
hunsD x = d 
  where xSTLast = x `div` 10 
        xLast   = xSTLast `div` 10 
        d       = xLast `mod` 10

hunsD' :: Integral a => a -> a
hunsD' = flip mod 10 . flip div 100

-- 2
foldBool :: a -> a -> Bool -> a
foldBool x y b = case b of
    False -> x
    True -> y

foldBool' :: a -> a -> Bool -> a
foldBool' x y b
  | b = x
  | otherwise = y

-- 3
g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)