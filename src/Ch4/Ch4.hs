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
