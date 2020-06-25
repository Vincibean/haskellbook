module Ch6.Ch6 where

import Data.List (sort)

-- Eq Instances
-- 1.
data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
    (TisAn i) == (TisAn j) = i == j

-- 2.
data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
    (Two i1 i2) == (Two j1 j2) = i1 == j1 && i2 == j2

-- 3.
data StringOrInt = TisAnInt Int | TisAString String

instance Eq StringOrInt where
    (TisAnInt i) == (TisAnInt j) = i == j
    (TisAString s) == (TisAString t) = s == t
    _ == _ = False

-- 4.
data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
    (Pair a b) == (Pair x y) = (a, b) == (x, y)

-- 5.
data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
    (Tuple a b) == (Tuple c d) = (a, b) == (c, d)

-- 6.
data Which a = ThisOne a | ThatOne a

instance Eq a => Eq (Which a) where
    ThisOne a == ThisOne b = a == b
    ThatOne a == ThatOne b = a == b
    _ == _ = False

-- 7.
data EitherOr a b = Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (Hello x) == (Hello y) = x == y
    (Goodbye x) == (Goodbye y) = x == y
    _ == _ = False

-- Tuple Experiment
-- Look at the types given for quotRem and divMod. What do you think those functions do?
--   divMod gives back a tuple (of the same input type a) representing the result of `div` and `mod` respectively 
--   (integer division and integer modulus).
--   Similarly, quotRem gives back a tuple (of the same input type a) representing the result of `quot` and `rem` respectively 
--   (integer division and integer remainder).

-- Put on your thinking cap
-- Why didn’t we need to make the type of the function we wrote require both typeclasses? Why didn’t we have to do this:
-- f :: (Num a, Fractional a) => a -> a -> a
--   Similarly to Liskov substitution principle, if we know that a Fractional "is-a" Num, we can use Num's functions whenever
--   Fractional is given. Fractional is a subset (subtype) of Num, thus "inheriting" all its functions, so Num a in (Num a, Fractional a)
--   is redundant and we can remove it.

-- Will They Work?
-- 1. max (length [1, 2, 3]) (length [8, 9, 10, 11, 12])
--    max works on types that have an instance of the Ord typeclass. length returns Int, which is a type that has an instance of the Ord
--    typeclass, so the preceding code will work; it will simplfy to: max (3 :: Int) (5 :: Int) => 5 :: Int
-- 2. compare (3 * 4) (3 * 5)
--    compare works on types that have an instance of the Ord typeclass. (*) works on types that have an instance of the Num typeclass.
--    There is no relationship between these two typeclasses, so in this case the typeclass will default to a concrete type. 
--    The default types are already set in the libraries, and, for Num a, the default is Integer; Integer has an instance of the Ord typeclass,
--    so it can be used in compare. The result will be: compare (12 :: Integer) (15 :: Integer) => LT (whose type is Ordering)
-- 3. compare "Julie" True
--    compare works on types that have an instance of the Ord typeclass. Both "Julie" and True have, but compare requires that both values
--    should be of the same type. "Julie" is of type String while True is of type Bool, so this expression won't compile.
-- 4. (5 + 3) > (3 + 6)
--    (>) works on types that have an instance of the Ord typeclass. (+) works on types that have an instance of the Num typeclass.
--    There is no relationship between these two typeclasses, so in this case the typeclass will default to a concrete type. 
--    The default types are already set in the libraries, and, for Num a, the default is Integer; Integer has an instance of the Ord typeclass,
--    so it can be used in (>). The result will be: (8 :: Integer) > (9 :: Integer) => false

-- Multiple choice
-- 1. The Eq class
--    c) makes equality tests possible
-- 2. The typeclass Ord
--    a) allows any two values to be compared
--    b) is a subclass of Eq
-- 3. Suppose the typeclass Ord has an operator > . What is the type of > ?
--    a) Ord a => a -> a -> Bool
-- 4. In x = divMod 16 12 
--    c) the type of x is a tuple
-- 5. The typeclass Integral includes
--    a) Int and Integer number

-- Does it typecheck?
-- 1.
-- Won't typecheck: there's no instance of Show (required by show) defined for Person. It can be fixed with:
data Person = Person Bool deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

-- 2.
-- Won't typecheck: there's no instance of Eq (required by (==)) defined for Mood. It can be fixed with:
data Mood = Blah | Woot deriving (Show, Eq)

settleDown x = if x == Woot 
                 then Blah
                 else x

-- 3. settleDown
--   a) What values are acceptable inputs to that function?
--      Any value of type Mood (that is, either Blah or Woot). if-then-else is an expression in Haskell. By providing a Blah in one of
--      the branches, we fix the value to be Mood.
--   b) What will happen if you try to run settleDown 9? Why?
--      We would get a compile time exception: as previously stated, only a value of type Mood is an acceptable input for the function.
--   c) What will happen if you try to run Blah > Woot? Why?
--      We would get a compile time exception, this itme because there is no instance of Ord (required by (>)) defined for the type Mood.
--      This can be fixed with:  data Mood = Blah | Woot deriving (Show, Eq, Ord)
--      Since Ord is a subtype of Eq, is can also be fixed with:  data Mood = Blah | Woot deriving (Show, Eq, Ord)

-- 4.
-- Will typecheck. There is nothing that dictates any particular constraint here. s2 is easy to understand. s1 is a function Object -> Sentence.
-- This is because even the constructor in Haskell is a function like everything else, and as such it can be curried.
-- Interestingly, the typeclass derivation here works because all the parameters of the Sentence data type belong to type with instances of
-- those typeclasses defined.   
type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"

-- Given a datatype declaration, what can we do?
data Rocks = Rocks String deriving (Eq, Show)

data Yeah = Yeah Bool deriving (Eq ,Show)

data Papu = Papu Rocks Yeah deriving (Eq, Show)

-- 1. 
-- phew = Papu "chases" True

-- Won't typecheck: the parameter types are completely different: the first one should be an instance of Rocks (which in turn requires a String); 
-- the second one should be an instance of Yeah (which in turn requires a Bool).
-- If Rocks and Yeah were type aliases instead of new type, that would have typechecked.

-- 2. 
truth = Papu (Rocks "chomskydoz") (Yeah True)

-- Typechecks just fine

-- 3.
equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

-- Typechecks: Papu derives Eq

-- 4.
-- comparePapus :: Papu -> Papu -> Bool
-- comparePapus p p' = p > p

-- Won't typecheck: Papu doesn't derive Ord. Interestigly, since Ord is a subclass of Eq, making Papu derive Eq will make this 
-- (as well as the previuos case) typecheck.

-- Match the types
-- 1.
i :: Num a => a
i = 1
-- Can't be substituted. 1 is a Num and a is too generic for it.

-- 2.
f :: Float
f = 1.0
-- Can't be substituted: 1.0 is a Fractional and Num is too generic for it.

-- 3.
f' :: Fractional a => a
f' = 1.0
-- Can be substituted: Fractional is generic enough (but not too generic) for 1.0.

-- 4.
f'' :: RealFrac a => a
f'' = 1.0
-- Can be substituted: RealFrac is a subtype of Fractional, so it can be used as a type whenever 
-- Fractional can. However, RealFrac is maybe too specific and it might be a good idea to stick with
-- the most generic type (Fractional)

-- 5.
freud :: Ord a => a -> a
freud x = x
-- Can be subsituted. In this case, we are putting in place a contraint (ad-hoc polymorphism) that, while valid, is not needed.

-- 6. 
freud' :: Int -> Int
freud' x = x
-- Can be subsituted. In this case, we are putting in place a contraint (the input and output type must be Int, specifically)
-- that, while valid, is not needed.

-- 7.
myX = 1 :: Int

sigmund :: Int -> Int
sigmund x = myX
-- Can't be substituted. myX is an Int, so the return type of sigmund must be an Int, too. We can't use something more generic.
-- However, we could have used sigmund :: a -> Int since we aren't doing anything with the input.

-- 8.
sigmund' :: Int -> Int
sigmund' x = fromIntegral myX
-- Can't be substituted. myX is an Int, so the return type of sigmund must be an Int, too. We can't use something more generic.

-- 9.
jung :: [Int] -> Int
jung xs = head (sort xs)
-- Can be substituted. In this case we are going from something generic to something more specific; moreover, we don't need the
-- typeclass constraint anymore since we know that Int has an instance of the Ord typeclass.

-- 10.
young :: Ord a => [a] -> a
young xs = head (sort xs)
-- Can be substituted. We are going from something specific to something generic, but on the other hand we weren't using that specificity,
-- so there's no risk to break anything.
-- Interestingly, here the typeclass consraint is needed.

-- 11.
mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
signifier xs = head (mySort xs)
-- Can't be substituted. mySort takes a String and returns a String, thus fixing the input type of signifier to String 
-- and the output to Char.

-- Type-Kwon-Do Two: Electric Typealoo

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f a b = f a == b

arith :: Num b => (a -> b) -> Integer -> a -> b
arith f i a = f a + fromIntegral i