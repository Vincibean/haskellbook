module Ch4.Ch4 where

-- Exercises: Mood Swing
-- Given the following datatype, answer the following questions:

data Mood = Blah | Woot deriving Show

-- 1. What is the type constructor, or name of this type?
-- Blah

-- 2. If the function requires a Mood value, what are the values you could possibly use there?
-- Blah; Woot

-- 3. We are trying to write a function changeMood to change Chris’s mood instantaneously.
-- It should act like not in that, given one value, it returns the other value of the same type.
-- So far, we’ve written a type signature changeMood :: Mood -> Woot. What’s wrong with that?
-- Type signatures always make reference to the type constructor, or datatypename.

-- 4. Now we want to write the function that changes his mood.
-- 5. Enter all of the above — datatype (including the deriving Show bit), your corrected type signature, 
-- and the corrected function into a source file.
changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood _    = Blah

-- Exercises: Find the Mistakes
-- The following lines of code may have mistakes — some of them won’t compile! You know what you need to do.
-- 1. not True && true
ftm1 = not True && True
-- 2. not (x = 6)
ftm2 = let x = 6 in not (x == 6)
-- 3. (1 * 2) > 5
ftm3 = (1 * 2) > 5 -- same
-- 4. [Merry] > [Happy]
ftm4 = ["Merry"] > ["Happy"]
-- 5. [1, 2, 3] ++ "look at me!"
ftm5 = ['1', '2', '3'] ++ "look at me!"

-- Chapter Exercises
awesome = ["Papuchon", "curry", ":)"]
alsoAwesome = ["Quake", "The Simons"]
allAwesome = [awesome, alsoAwesome]
-- length is a function that takes a list and returns a result that tells how many items are in the list.

-- 1.Given the definition of length above, what would the type signature be? 
-- How many arguments, of what type does it take? What is the type of the result it evaluates to?
length' :: (Foldable t, Integral i) => t a -> i
length' = undefined

-- 2. What are the results of the following expressions?
-- a) length [1, 2, 3, 4, 5]
-- 5
-- b) length [(1, 2), (2, 3), (3, 4)]
-- 3
-- c) length allAwesome
-- 2
-- d) length (concat allAwesome)
-- 5

-- 3. Look at these two expressions. One works and one returns an error. Determine which will return an error and why.
-- Prelude> 6 / 3
-- Prelude> 6 / length [1, 2, 3]
-- (/) is contrained to types that have an instance of a Fractional typeclass. In the first case (6 / 3), the type of 3 
-- isn't specified, so it can be considered a Fractional. In the second case (6 / length [1, 2, 3]), length returns an Int,
-- and Int doesn't hae an instance of Fractional, so this case throws a compile time error.

-- 4. How can you fix the broken code from the preceding exercise using a different division function/operator?
ce4 = 6 `div` length [1, 2, 3]

-- 5. What is the type of the expression 2 + 3 == 5? What would we expect as a result?
-- Bool. True

-- 6. What is the type and expected result value of the following:
-- Prelude> let x = 5
-- Prelude> x + 3 == 5
-- The first expression (let x = 5) defines a number, so the type will be Num => Num a; the second one is a comparison between two numbers,
-- so the type will be Bool and the result will be False.

-- 7.Below are some bits of code. Which will work? Why or why not? 
-- If they will work, what value would these reduce to?
-- Prelude> length allAwesome == 2
--   This will work. The result will be 2
-- Prelude> length [1, 'a', 3, 'b']
--   This will not work: length can only contain elements of the same type, while this list contain both numbers and characters.
-- Prelude> length allAwesome + length awesome
--   This will work. The result will be 5
-- Prelude> (8 == 8) && ('b' < 'a')
--   This will work. (8 == 8) will evaluate to True; ('b' < 'a') will evaluate to False; the end result will be False.
-- Prelude> (8 == 8) && 9
--   This will not work: && only works with booleans, and the second argument here (9) is a number.

-- 8. Write a function that tells you whether or not a given String (or list) is a palindrome. 
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

-- 9. Write a function to return the absolute value of a number using if-then-else
myAbs :: Integer -> Integer
myAbs i = if (i < 0)
          then -i
          else i

-- 10. Fill in the definition of the following function, using fst and snd
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f t1 t2 = ((snd t1, snd t2), (fst t1, fst t2))

-- Correcting syntax
-- 1. 
x = (+)

csf1 xs = w `x` 1
  where w = length xs

-- 2.
id' x = x -- or, as a lamda: (\x -> x)

-- 3.
cs3 (x:xs) = x -- or, as a lambda: (\(x:xs) -> x)

-- 4.
csf4 (a,b) = a

-- Match the function names to their types

-- 1. Which of the following types is the type of show?
-- c) Show a => a -> String

-- 2. Which of the following types is the type of (==)?
-- b) Eq a => a -> a -> Bool

-- 3. Which of the following types is the type of fst?
-- a)(a, b) -> a

-- 4. Which of the following types is the type of (+)?
-- d)(+) :: Num a => a -> a -> a