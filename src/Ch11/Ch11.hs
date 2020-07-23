{-# LANGUAGE FlexibleInstances #-}

module Ch11.Ch11 where

import           Data.Char
import           Data.Int
import           Data.List (group, intercalate, maximumBy, sort)
import Data.Function


data PugType = PugData

data HuskyType a = HuskyData

data DogueDeBordeaux doge = DogueDeBordeaux doge

data Doggies a = Husky a | Mastiff a deriving (Eq, Show)

-- Exercises: Dog Types
-- Given the datatypes defined in the above sections,
-- 1. Is Doggies a type constructor or a data constructor?
--    Type constructor
-- 2. What is the kind of Doggies?
--    Doggies :: * -> *
-- 3. What is the kind of Doggies String?
--    Doggies String :: *
-- 4. What is the type of Husky 10?
--    Husky 10 :: Num a => Doggies a
-- 5. What is the type of Husky (10 :: Integer)?
--    Husky (10 :: Integer) :: Doggies Integer
-- 6. What is the type of Mastiff "Scooby Doo"?
--    Mastiff "Scooby Doo" :: Doggies [Char]
-- 7. Is DogueDeBordeaux a type constructor or a data constructor?
--    Both
-- 8. What is the type of DogueDeBordeaux?
--    DogueDeBordeaux :: doge -> DogueDeBordeaux doge
-- 9. What is the type of DogueDeBordeaux "doggie!"
--    DogueDeBordeaux "doggie!" :: DogueDeBordeaux [Char]

-- Exercises: Vehicles

data Price = Price Integer deriving (Eq, Show)

data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)

data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq, Show)

data Vehicle = Car Manufacturer Price | Plane Airline deriving (Eq, Show)

myCar = Car Mini (Price 14000)

urCar = Car Mazda (Price 20000)

clownCar = Car Tata (Price 7000)

doge = Plane PapuAir

-- 1. What is the type of myCar?
--    myCar :: Vehicle
-- 2. Given the following, define the functions:

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _         = False

isPlane :: Vehicle -> Bool
isPlane (Plane _) = True
isPlane _         = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

-- 3. Now we’re going to write a function to tell us the manufacturer of a piece of data:

getManu :: Vehicle -> Manufacturer
getManu (Car manuf _) = manuf
getManu _ = error "can't do anything but fail if we want to keep this signature!"

-- 4. Given that we’re returning the Manufacturer, what will happen if you use this on Plane data?
--    It will fail (go bottom) with an informative error message explaining that I didn't have any other choice :(
--    If we don't want it to fail, we should rewrite it as:

getManu' :: Vehicle -> Maybe Manufacturer
getManu' (Car m _) = Just m
getManu' _         = Nothing  

-- 5. Let’s say you’ve decided to add the size of the plane as an argument to the Plane constructor.
--    Add that to your datatypes in the appropriate places and change your data and functions appropriately.

data Size = XS | S | M | L | XL deriving (Eq, Show)

data Vehicle' = Car' Manufacturer Size Price | Plane' Airline Size deriving (Eq, Show)

isCar' :: Vehicle' -> Bool
isCar' (Car' _ _ _) = True
isCar' _            = False

isPlane' :: Vehicle' -> Bool
isPlane' (Plane' _ _) = True
isPlane' _            = False

areCars' :: [Vehicle'] -> [Bool]
areCars' = map isCar'

getManu'' :: Vehicle' -> Manufacturer
getManu'' (Car' manuf _ _) = manuf
getManu'' _ = error "can't do anything but fail if we want to keep this signature!"

getSize :: Vehicle' -> Size
getSize (Car' _ s _) = s
getSize (Plane' _ s) = s

-- Exercises: Cardinality

-- 1. data PugType = PugData
--    Cardinality: 1

-- 2.  data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited
--    Cardinality: 3

-- 3. Given what we know about Int8, what’s the cardinality of Int16?
--    Cardinality Int16 = Cardinality Int8 ^ 2 = 256 ^ 2  == 65536

-- 4. Use the REPL and maxBound and minBound to examine Int and Integer. What can you say about the cardinality of those types?
--    Cardinality Int = "Int" is the more common 32 or 64 bit integer. Implementations vary, although it is guaranteed to be at least 30 bits.
--    Cardinality Integer = "Integer" is an arbitrary precision type: it will hold any number no matter how big, up to the limit of your machine's memory
--    Source: The Haskell Wikibook

-- 5. Extra credit (impress your friends!): What’s the connection between the 8 in Int8 and that type’s cardinality of 256?
--    2 ^ 8 = 256
--    In other words, 8 here is the number of bits needed to store a number whose type is Int8


-- Exercises: For Example

data Example = MakeExample deriving Show

-- 1. What is the type of data constructor MakeExample? What happens when you request the type of Example?
--    λ>:t MakeExample
--    MakeExample :: Example
--    λ>:t Example
--    <interactive>:1:1: error: Data constructor not in scope: Example

-- 2. What if you try :info on Example in GHCi? Can you determine what typeclass instances are defined?
--    data Example = MakeExample
--      	-- Defined at /home/dre/projects/haskellbook/src/Ch11/Ch11.hs:123:1
--    instance [safe] Show Example
--      -- Defined at /home/dre/projects/haskellbook/src/Ch11/Ch11.hs:123:37

--    Which means that only Show is defined for Example (as expected)

-- 3. Try making a new datatype like Example but with a single type argument added to MakeExample, such as Int. What has changed when you query MakeExample with :type in GHCi?

data Example' = MakeExample' String deriving Show

--    λ>:t MakeExample'
--    MakeExample' :: String -> Example'

-- Exercises: Logic Goats

-- 1. Reusing the TooMany typeclass, write an instance of the typeclass for the type (Int, String)

class TooMany a where
  tooMany :: a -> Bool

instance TooMany (Int, String) where
  tooMany (n,s) = n > 42 && length s > 5

-- 2. Make another TooMany instance for (Int, Int). Sum the values together under the assumption this is a count of goats from two fields.

instance TooMany (Int, Int) where
  tooMany (n1,n2) = n1 + n2 > 42

-- 3. Make another TooMany instance, this time for (Num a, TooMany a) => (a, a). This can mean whatever you want, such as summing the two numbers together.

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (n1,n2) = tooMany $ n1 + n2


-- Exercises: Pity the Bool

-- 1. Given a datatype
data BigSmall = Big Bool | Small Bool deriving (Eq, Show)
--    What is the cardinality of this datatype?
--    Big Bool | Small Bool == Big Bool + Small Bool == Bool + Bool == 2 + 2 == 4

-- 2. Given a datatype
data NumberOrBool = Numba Int8 | BoolyBool Bool deriving (Eq, Show)
--    What is the cardinality of NumberOrBool?
--    Numba Int8 | BoolyBool Bool == Numba Int8 + BoolyBool Bool == Int8 + Bool == 256 + 2 = 258
-- What happens if you try to create a Numba with a numeric literal larger than 127? And with a numeric literal smaller than (-128)?
--    <interactive>:52:18: warning: [-Woverflowed-literals]
--    Literal 129 is out of the GHC.Int.Int8 range -128..127

-- Exercises: How Does Your Garden Grow?
-- 1. Given the type
-- data FlowerType = Gardenia | Daisy | Rose | Lilac deriving Show

-- type Gardener = String

-- data Garden = Garden Gardener FlowerType deriving Show
-- What is the normal form of Garden?
--   It must be a sum (type) of products, so:
type Gardener = String
data Garden = Gardenia Gardener | Daisy Gardener | Rose Gardener | Lilac Gardener deriving Show

-- Exercise: Programmers

data OperatingSystem = GnuPlusLinux | OpenBSDPlusNevermindJustBSDStill | Mac | Windows deriving (Eq, Show)

data ProgrammingLanguage = Haskell | Agda | Idris | PureScript deriving (Eq, Show)

data Programmer = Programmer { os :: OperatingSystem, lang :: ProgrammingLanguage } deriving (Eq, Show)

-- Write a function that generates all possible values of Programmer.

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [ GnuPlusLinux, OpenBSDPlusNevermindJustBSDStill, Mac, Windows ]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [Programmer os lang | os <- allOperatingSystems, lang <- allLanguages]

-- Exponentiation in what order?

data Quantum = Yes | No | Both deriving (Eq, Show)

-- Consider the following function:
convert :: Quantum -> Bool
convert = undefined
-- According to the equality of a -> b and b^a there should be 2 ^ 3 or 8 implementations of this function. Does this hold? Write it out and prove it for yourself.

convert1 :: Quantum -> Bool
convert1 Yes  = True
convert1 No   = True
convert1 Both = True

convert2 :: Quantum -> Bool
convert2 Yes  = True
convert2 No   = True
convert2 Both = False

convert3 :: Quantum -> Bool
convert3 Yes  = True
convert3 No   = False
convert3 Both = False

convert4 :: Quantum -> Bool
convert4 Yes  = False
convert4 No   = False
convert4 Both = False

convert5 :: Quantum -> Bool
convert5 Yes  = False
convert5 No   = False
convert5 Both = True

convert6 :: Quantum -> Bool
convert6 Yes  = False
convert6 No   = True
convert6 Both = True

convert7 :: Quantum -> Bool
convert7 Yes  = False
convert7 No   = True
convert7 Both = False

convert8 :: Quantum -> Bool
convert8 Yes  = True
convert8 No   = False
convert8 Both = True

-- Exercises: The Quad

data Quad = One | Two | Three | Four deriving (Eq, Show)

-- 1. Determine how many unique inhabitants each type has
--    eQuad :: Either Quad Quad
--                      4 +  4  = 8

--    prodQuad :: (Quad, Quad)
--                    4 x  4  = 16

--    funcQuad :: Quad -> Quad
--                  4  ^   4  = 256

--    prodTBool :: (Bool, Bool, Bool)
--                    2  x  2  x  2 = 8

--    gTwo :: Bool -> Bool -> Bool
--    (Bool ^ Bool) ^ Bool == Bool ^ (Bool * Bool) == 2 ^ (2 * 2) == 2 ^ 4 == 16

--    fTwo :: Bool -> Quad -> Quad
--    (Quad ^ Quad) ^ Bool ==  Quad ^ (Quad * Bool) == 4 ^ (4 * 2) == 4 ^ 8 == 65536


data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a = Node (insert' b left) a right
  | b > a = Node left a (insert' b right)

-- Write map for BinaryTree
-- filling in some details to help you along
-- Note, you do *not* need to use insert' for this.
-- Retain the original structure of the tree.
mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf                = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)
-- acceptance test for mapTree
mapOkay =
  if mapTree (+1) testTree' == mapExpected
  then print "yup okay!"
  else error "test failed!"


-- Convert binary trees to lists

preorder :: BinaryTree a -> [a]
preorder Leaf                = []
preorder (Node left a right) = a : preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf                = []
inorder (Node left a right) = inorder left ++ a : inorder right

postorder :: BinaryTree a -> [a]
postorder Leaf                = []
postorder (Node left a right) = postorder left ++ postorder right ++ [a]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
    if preorder testTree == [2, 1, 3]
    then putStrLn "Preorder fine!"
    else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
    if inorder testTree == [1, 2, 3]
    then putStrLn "Inorder fine!"
    else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
    if postorder testTree == [1, 3, 2]
    then putStrLn "Postorder fine!"
    else putStrLn "postorder failed check"

main :: IO ()
main = do
    testPreorder
    testInorder
    testPostorder


-- Write foldr for BinaryTree

-- postorder
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ b Leaf = b
foldTree f b (Node left a right) = f a $ foldTree f l right
  where l = foldTree f b left

-- preorder
foldTree' :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree' _ b Leaf = b
foldTree' f b (Node left a right) = 
  foldTree' f (foldTree' f (f a b) left) right

-- inorder
foldTree'' :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree'' _ b Leaf = b
foldTree'' f b (Node left a right) =
     foldTree'' f (f a (foldTree'' f b left)) right

-- Chapter Exercises

-- Multiple choice

-- 1. Given the following datatype:
data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday
--    a) Weekday is a type with five data constructors

-- 2. what is the type of the following function, f?
f Friday = "Miller Time"
--    c) f :: Weekday -> String

-- 3. Types defined with the data keyword
--    b) must begin with a capital letter

-- 4. The function g xs = xs !! (length xs - 1)
--    c) Returns the final element of xs


-- As-patterns
-- 1. This should return True if (and only if ) all the values in the first list appear in the second list, though they need not be contiguous.
isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf s1@(h1:t1) (h2:t2) = (h1 == h2 && isSubsequenceOf t1 t2) || isSubsequenceOf s1 t2

-- 2. Split a sentence into words, then tuple each word with the capitalized form of each.
capitalizeWords :: String -> [(String, String)]
capitalizeWords s = map f $ words s
  where f :: String -> (String, String)
        f ""      = ("", "")
        f s@(h:t) = (s, toUpper h : t)

-- Language exercises
-- 1. Write a function that capitalizes a word.
capitalizeWord :: String -> String
capitalizeWord ""    = ""
capitalizeWord (h:t) = toUpper h : t

-- 2. Write a function that capitalizes sentences in a paragraph.
capitalizeParagraph :: String -> String
capitalizeParagraph = (++ ".") . intercalate ". " . map capitalizeWord . sentences

sentences :: String -> [String]
sentences s = case break (== '.') s of
    ("", "") -> []
    ("", x) -> [x]
    (x, xs) -> (x :) . sentences . dropWhile isSpaceOrDot $ xs
      where isSpaceOrDot x = isSpace x || x == '.'

-- Phone exercise

-- 1. Create a data structure that captures the phone layout above

-- validButtons = "1234567890*#"
type Digit = Char

-- Valid presses: 1 and up
type Presses = Int

data DaPhone = DaPhone (Char -> [(Digit, Presses)])

mkDaPhone = DaPhone f
  where f 'a' = [('2', 1)]
        f 'b' = [('2', 2)]
        f 'c' = [('2', 3)]
        f 'd' = [('3', 1)]
        f 'e' = [('3', 2)]
        f 'f' = [('3', 3)]
        f 'g' = [('4', 1)]
        f 'h' = [('4', 2)]
        f 'i' = [('4', 3)]
        f 'j' = [('5', 1)]
        f 'k' = [('5', 2)]
        f 'l' = [('5', 3)]
        f 'm' = [('6', 1)]
        f 'n' = [('6', 2)]
        f 'o' = [('6', 3)]
        f 'p' = [('7', 1)]
        f 'q' = [('7', 2)]
        f 'r' = [('7', 3)]
        f 's' = [('7', 4)]
        f 't' = [('8', 1)]
        f 'u' = [('8', 2)]
        f 'v' = [('8', 3)]
        f 'w' = [('9', 1)]
        f 'x' = [('9', 2)]
        f 'y' = [('9', 3)]
        f 'z' = [('9', 4)]
        f ' ' = [('0', 2)]
        f '.' = [('#', 1)]
        f ',' = [('#', 2)]
        f '1' = [('1', 1)]
        f '2' = [('2', 4)]
        f '3' = [('3', 4)]
        f '4' = [('4', 4)]
        f '5' = [('5', 4)]
        f '6' = [('6', 4)]
        f '7' = [('7', 5)]
        f '8' = [('8', 4)]
        f '9' = [('9', 5)]
        f x = if isUpper x then ('*', 1) : f (toLower x) else []

convo :: [String]
convo =
  ["Wanna play 20 questions",
   "Ya",
   "U 1st haha",
   "Lol ok. Have u ever tasted alcohol lol",
   "Lol ya",
   "Wow ur cool haha. Ur turn",
   "Ok. Do u think I am pretty Lol",
   "Lol ya",
   "Haha thanks just making sure rofl ur turn"]

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone f) c = f c

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead phone s = s >>= reverseTaps phone

-- 3. How many times do digits need to be pressed for each message?
fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . map (snd)

-- 4. What was the most popular letter for each message? What was its cost?
mostPopularLetter :: String -> Char
mostPopularLetter = mostCommon

mostPopularLetterCost :: DaPhone -> String -> Presses
mostPopularLetterCost (DaPhone f) = fingerTaps . f . mostCommon

-- 5. What was the most popular letter overall? What was the most popular word?
coolestLtr :: [String] -> Char
coolestLtr = mostCommon . concat

coolestWord :: [String] -> String
coolestWord = mostCommon

mostCommon :: Ord a => [a] -> a
mostCommon = head . maximumBy (compare `on` length) . group . sort

-- Hutton’s Razor
-- 1. Your first task is to write the “eval” function which reduces an expression to a final sum.

data Expr = Lit Integer | Add Expr Expr

eval :: Expr -> Integer
eval (Lit i) = i
eval (Add expr1 expr2) = eval expr1 + eval expr2

-- 2. Write a printer for the expressions.
printExpr :: Expr -> String
printExpr (Lit i) = show i
printExpr (Add expr1 expr2) = printExpr expr1 ++ " + " ++ printExpr expr2