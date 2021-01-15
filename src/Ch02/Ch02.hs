module Ch02.Ch02 where

-- Exercises: A Head Code

-- Rewrite with where clauses:
-- 1. let x = 3; y = 1000 in x * 3 + y
a = x * 3 + y
  where x = 3
        y = 1000
-- 2. let y = 10; x = 10 * 5 + y in x * 5
b = x * 5
  where y = 10
        x = 10 * 5 + y
        
-- 3. let x = 7; y = negate x; z = y * 10 in z / x + y
c = z / x + y
  where x = 7
        y = negate x
        z = y * 10

-- Exercises: More fun with functions
-- 4. Rewrite waxOn as an expression with a where clause in your source file. 
--    Load it into your REPL and make sure it still works as expected!
waxOn = x * 5
  where x = y ^ 2
        y = z + 8
        z = 7

-- 5. Now to the same source file where you have waxOn, add the triple function.
triple x= x * 3

-- 6. Now, without changing what you’ve done so far in that file, add a new function called waxOff
waxOff x = triple x