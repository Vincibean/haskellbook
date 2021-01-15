module Ch05.Ch05 where

-- Exercises: Type Matching
-- Functions:
-- a)not
--   not :: Bool -> Bool
-- b)length
--   length :: [a] -> Int
-- c)concat
--   concat :: [[a]] -> [a]
-- d)head
--   head :: [a] -> a
-- e)(<)
--   (<) :: Ord a => a -> a -> Bool

-- Exercises: Type Arguments
-- Given a function and its type, tell us what type results from applying some or all of the arguments.

-- 1. If the type of f is a -> a -> a -> a, and the type of x is Char then the type off x is
--    a) Char -> Char -> Cha

-- 2. If the type of g is a -> b -> c -> b, then the type of g 0 'c' "woot" is
--    d) Char

-- 3. If the type of h is (Num a, Num b) => a -> b -> b, then the type of h 1.0 2 is
--    d) Num b => b

-- 4. If the type of h is (Num a, Num b) => a -> b -> b, then the type of h 1 (5.5 :: Double) is
--    c) Double

-- 5. f the type of jackal is (Ord a, Eq b) => a -> b -> a, then the type of jackal "keyboard" "has the word jackal in it"
--    a) [Char]

-- 6. If the type of jackal is (Ord a, Eq b) => a -> b -> a, then the type of jackal "keyboard"
--    e) Eq b => b -> [Char]

-- 7. If the type of kessel is (Ord a, Num b) => a -> b -> a, then the type of kessel 1 2 is
--    d) (Num a, Ord a) => a

-- 8. If the type of kessel is (Ord a, Num b) => a -> b -> a, then the type of kessel 1 (2 :: Integer) is
--    a) (Num a, Ord a) => a

-- 9. If the type of kessel is (Ord a, Num b) => a -> b -> a, then the type of kessel 1 (2 :: Integer) is
--    c) Integer

-- Exercises: Parametricity
-- 2. The hypothetical function a -> a -> a has two – and only two – implementations.
-- Write both possible versions of a -> a -> a.
iid :: a -> a -> a
iid a1 _ = a1

iid' :: a -> a -> a
iid' _ a2 = a2

-- 3. Implement a -> b -> b.  How many implementations can it have?
iid'' :: a -> b -> b
iid'' _ b = b

-- Does the behavior change when the types of a and b change?
--   No.

-- Exercises: Apply Yourself
-- 1. The signature will be: myConcat :: [Char] ++ [Char]
--    The compiler knows the function (++) and has one value to work with already that it knows is a [Char].

-- 2. Two instances of Num are given, but the operation that we apply are (*) (which is contrained to Num) and (/) (which is
--    constrained to Fractional). Hence, the input type must have an instance of Fractional.

-- 3. myTake's first argument is passed to take, whose first argument is an Int, so myTake's first argument must be Int.

-- 4. length returns a Int, and it is applied as the second argument of (<). This "fixes" the first argument to be of the same type.

-- 5. Similarly as before, here the second argument of (<) is a Char. This "fixes" the first argument to be of the same type.

-- Multiple choice
-- 1. A value of type [a] is
--    c) a list whose elements are all of some type a

-- 2. A function of type [[a]] -> [a] could
--    a) take a list of strings as an argument

-- 3. A function of type [a] -> Int -> a
--    b) returns one element of type a from a list

-- 4. A function of type (a, b) -> a
--    c) takes a tuple argument and returns the first value

-- Determine the type

-- 1.a. Num a => a
-- 1.b. Num a => (a, String)
-- 1.c. (Integer, String)
-- 1.d. Bool
-- 1.e. Int
-- 1.f. Bool

-- 2. Num a => a

-- 3. Num a => a -> a

-- 4. Fractional a => a -> a

-- 5. String

-- Does it compile?
-- 1.
bigNum = (^) 5
wahoo = bigNum $ 10

-- 2.
x = print
y = print "woohoo!"
z = x "hello world"

-- 3.
-- Won't compile; it can be fixed with:
a = (+)
b = a
c = b 10
d = c 200

-- 4
-- Won't compile
-- a = 12 + b
-- b = 10000 * c

-- Type variable or specific type constructor?
-- 2.
-- f :: zed -> Zed -> Blah
--      [0]    [1]    [2]
-- [0] fully polymorphic
-- [1] concrete
-- [2] concrete

-- 3.
-- f :: Enum b => a -> b -> C
--               [0]  [1]  [2]
-- [0] fully polymorphic
-- [1] constrained polymorphic
-- [2] concrete

-- 4.
-- f :: f -> g -> C
--     [0]  [1]  [2]
-- [0] fully polymorphic
-- [1] fully polymorphic
-- [2] concrete

-- Write a type signature
-- 1.
-- functionH :: [a] -> a

-- 2.
-- functionC :: Ord a => a -> a -> Bool

-- 3.
-- functionS :: (a, b) -> b

-- Given a type, write the function
-- 1.
i :: a -> a
i a = a

-- 2.
c' :: a -> b -> a
c' a _ = a

-- 3. Given alpha equivalence are c'' and c' (see above) the same thing?
--    Yes

-- 4.
c'' :: a -> b -> b
c'' _ b = b

-- 5.
r :: [a] -> [a]
r as = tail as

-- 6.
co :: (b -> c) -> (a -> b) -> a -> c
co bToC aToB a = bToC (aToB a)

co' :: (b -> c) -> (a -> b) -> a -> c
co' bToC aToB = bToC . aToB

co'' :: (b -> c) -> (a -> b) -> a -> c
co'' = (.)

-- 7.
a' :: (a -> c) -> a -> a
a' _ x = x

-- 8.
a'' :: (a -> b) -> a -> b
a'' f a = f a

a''' :: (a -> b) -> a -> b
a''' = ($)

-- Type-Kwon-Do
-- 1.

f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h i = g(f(i))

h' :: Int -> Char
h' = g . f

-- 2.
data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e a = w(q(a))

e' :: A -> C
e' = w . q

-- 3.
data X
data Y
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X,Y) -> (Z,Z)
xform (x, y) = (xz x, yz y)

xform' :: (X,Y) -> (Z,Z)
xform' (x, y) = (yz y, xz x)

xform'' :: (X,Y) -> (Z,Z)
xform'' (x, y) = (xz x, xz x)

xform''' :: (X,Y) -> (Z,Z)
xform''' (x, y) = (yz y, yz y)

-- 4.
munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge xToY yToWZ x = w
  where (w, z) = (yToWZ (xToY x))

munge' :: (x -> y) -> (y -> (w, z)) -> x -> w
munge' xToY yToWZ x = let (w, z) = (yToWZ (xToY x))
                      in w
