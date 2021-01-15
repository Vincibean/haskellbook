module Ch08.Ch08 where

-- Intermission: Exercise
-- Write out the evaluation of the following. It might be a little less noisy if you do so with the form that didn’t use (.).
-- applyTimes 5 (+1) 5
-- (+1) (applyTimes 4 (+1) 5)
-- (+1) ((+1) (applyTimes 3 (+1) 5))
-- (+1) ((+1) ((+1) (applyTimes 2 (+1) 5)))
-- (+1) ((+1) ((+1) ((+1) (applyTimes 1 (+1) 5))))
-- (+1) ((+1) ((+1) ((+1) ((+1) (applyTimes 0 (+1) 5)))))
-- (+1) ((+1) ((+1) ((+1) ((+1) 5))))
-- (+1) ((+1) ((+1) ((+1) 6)))
-- (+1) ((+1) ((+1) 7))
-- (+1) ((+1) 8)
-- (+1) 9
-- 10

-- Review of types
-- 1. What is the type of [[True, False], [True, True], [False, True]]?
--    d) [[Bool]]

-- 2. Which of the following has the same type as [[True, False], [True, True], [False, True]] ?
--    b) [[3 == 3], [6 > 5], [3 < 4]]

-- 3. For the following function
--      func :: [a] -> [a] -> [a]
--      func x y = x ++ y
--    which of the following is true?
--    d) all of the above

-- 4. For the func code above, which is a valid application of func to both of its arguments?
--    b) func "Hello" "World"

-- Reviewing currying
-- 1. What is the value of appedCatty "woohoo!" ?
--    "woops mrow woohoo!"

-- 2. frappe "1"
--    "1 mrow haha"

-- 3. frappe (appedCatty "2")
--    "woops mrow 2 mrow haha"

-- 4. appedCatty (frappe "blue")
--    "woops mrow blue mrow haha"

-- 5. cattyConny (frappe "pink") (cattyConny "green" (appedCatty "blue"))
--    "pink mrow haha mrow green mrow woops mrow blue"

-- 6. cattyConny (flippy "Pugs" "are") "awesome"
--    "are mrow Pugs mrow awesome"

-- Recursion
-- 1. Write out the steps for reducing dividedBy 15 2 to its final answer according to the Haskell code.

-- dividedBy 15 2 =
-- go 15 2 0
--   | 15 < 2 = ... -- false, skip this branch
--   |otherwise = go (15 - 2) 2 (0 + 1)
--   -- otherwise is literally the value True
--   -- so if first branch fails, this always succeeds
--       go 13 2 1
--         go (13 - 2) 2 (1 + 1)
--           -- 13 isn't < 2, so the otherwise branch
--           -- n == 11, d == 2, count == 2
--
--         go 11 2 2
--           go (11 - 2) 2 (2 + 1)
--             -- 11 isn't < 2, so the otherwise branch
--             -- n == 9, d == 2, count == 3
--
--           go 9 2 3
--             go (9 - 2) 2 (3 + 1)
--               -- 9 isn't < 2, so the otherwise branch
--               -- n == 7, d == 2, count == 4
--
--             go 7 2 4
--               go (7 - 2) 2 (4 + 1)
--                 -- 7 isn't < 2, so the otherwise branch
--                 -- n == 5, d == 2, count == 5
--
--               go 5 2 5
--                 go (5 - 2) 2 (5 + 1)
--                   -- 5 isn't < 2, so the otherwise branch
--                   -- n == 3, d == 2, count == 6
--
--                 go 3 2 6
--                   go (3 - 2) 2 (6 + 1)
--                     -- 3 isn't < 2, so the otherwise branch
--                     -- n == 1, d == 2, count == 7
--
--                   go 1 2 7
--                     go (3 - 2) 2 (6 + 1)
--                       -- the n < d branch is finally evaluated
--                       -- because 1 < 2 is true
--                       -- n == 1, d == 2, count == 7
--                       | 1 < 2 = (7, 1)
-- (7, 1)


-- 2. Write a function that recursively sums all numbers from 1 to n, n being the argument.
sumAll :: (Eq a, Num a) => a -> a
sumAll 0 = 0
sumAll n = n + sumAll (n - 1)

-- 3. Write a function that multiplies two integral numbers using recursive summation. The type should be (Integral a) => a -> a -> a.
sumRec :: (Integral a) => a -> a -> a
sumRec x y
  | y <= 1 = x
  | otherwise = x + sumRec x (y - 1)

-- Fixing dividedBy
data DividedResult a = Result (a, a) | DividedByZero deriving Show

dividedBy :: Integral a => a -> a -> DividedResult a
dividedBy num denom = go num denom 0
  where go n d count
         | d == 0 = DividedByZero
         | n < 0 = negateDividedResult $ go (-n) d count
         | d < 0 = negateDividedResult $ go n (-d) count
         | n < d = Result (count, n)
         | otherwise = go (n-d) d (count + 1)

negateDividedResult :: Integral a => DividedResult a -> DividedResult a
negateDividedResult DividedByZero       = DividedByZero
negateDividedResult (Result (count, n)) = Result (negate count, n)

-- McCarthy 91 function
mc91 :: (Ord a, Num a) => a -> a
mc91 x
 | x > 100 = x - 10
 | otherwise = 91

mc91' :: (Ord a, Num a) => a -> a
mc91' x
 | x > 100 = x - 10
 | otherwise = mc91 . mc91 . (+11) $ x