module Ch14.Ch14Spec where

import Ch14.Ch14
import Test.Hspec
import Test.QuickCheck
import Data.Char (toUpper)

import Data.List (sort)

half x = x / 2

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t)      = (Just y, t)
        go y (Just x, t)       = (Just y, x >= y)

twice f = f . f

fourTimes = twice . twice

capitalizeWord :: String -> String
capitalizeWord ""    = ""
capitalizeWord (h:t) = toUpper h : t

spec :: Spec
spec = do
  describe "Validating numbers into words" $ do
    it "12 multiplied by 0 is 0" $ do
      sumRec 12 0 `shouldBe` 0

    it "12 multiplied by 1 is 12" $ do
      sumRec 12 1 `shouldBe` 12

    it "12 multiplied by 2 is 24" $ do
      sumRec 12 2 `shouldBe` 24

    it "-12 multiplied by 0 is 0" $ do
      sumRec (-12) 0 `shouldBe` 0

    it "-12 multiplied by 1 is -12" $ do
      sumRec (-12) 1 `shouldBe` (-12)

    it "1 multiplied by -12 is -12" $ do
      sumRec 1 (-12) `shouldBe` (-12)

    it "12 multiplied by -2 is -24" $ do
      sumRec 12 (-2) `shouldBe` 24

    it "-12 multiplied by 2 is -24" $ do
      sumRec (-12) 2 `shouldBe` 24

    it "-12 multiplied by -2 is 24" $ do
      sumRec (-12) (-2) `shouldBe` 24

-- Using QuickCheck
-- Test some simple arithmetic properties using QuickCheck.
-- 1. 
  describe "Using QuickCheck" $ do
    it "halfIdentity = (*2) . half" $ do
      property $ \x -> half (x * 2) == (x :: Double)
-- 2. 
    it "for any list you apply sort to this list ordered should hold" $ do
      property $ \xs -> listOrdered (sort (xs :: [Integer])) `shouldBe` True
-- 3. 
    it "addition is associative" $ do
      property $ \x y z -> x + (y + z) == (x + y) + (z :: Integer)
    it "addition is commutative" $ do
      property $ \x y -> x + y == y + (x :: Integer)
-- 4. 
    it "multiplication is associative" $ do
      property $ \x y z -> x * (y * z) `shouldBe` (x * y) * (z :: Integer)
    it "multiplication is commutative" $ do
      property $ \x y -> x * y `shouldBe` y * (x :: Integer)
-- 5.
    it "(quot x y)*y + (rem x y) should be x" $ do
      property $ \x y -> y /= 0 ==> (quot x y) * y + (rem x y) `shouldBe` (x :: Integer)
    it "(quot x y)*y + (rem x y) should be x" $ do
      property $ \x y -> y /= 0 ==>  (div x y)*y + (mod x y) `shouldBe` (x :: Integer)
-- 6. Both properties are falsifiable
    -- it "exponent is associative" $ do
    --   property $ \x y z -> x >= 0 && y >= 0 && z >= 0 ==> x ^ (y ^ z) `shouldNotBe` ((x :: Integer) ^ (y :: Integer)) ^ (z :: Integer)
    -- it "exponent is commutative" $ do
    --   property $ \x y -> x >= 0 && y >= 0 ==> x ^ y `shouldNotBe` y ^ (x :: Integer)
-- 7.
    it "reversing a list twice is the same as the identity of the list" $ do
      property $ \xs -> (reverse . reverse) xs `shouldBe` (xs :: [Integer])
-- 8.
    it "definition of ($)" $ do
      property $ \(Fn f) x -> ((f :: Integer -> Integer) $ x) === f x
    it "definition of (.)" $ do
      property $ \(Fn f) (Fn g) x -> ((f :: Integer -> Integer) . (g :: Integer -> Integer) $ x) === f (g x)
-- 9.
    it "flip (foldr (:)) == (++)" $ do
      property $ \xs ys -> flip (foldr (:)) xs ys === (++) (xs :: [Integer]) (ys :: [Integer])
    it "foldr (++) [] == concat" $ do
      property $ \xs -> foldr (++) [] xs === concat (xs :: [[Integer]])
-- 10.
    -- it "f n xs = length (take n xs) == n" $ do
    --   property $ \ (Positive n) xs -> length (take n (xs :: [Integer])) === (n :: Int)
-- 11.
    it "read . show == id" $ do
      property $ \x -> (read (show x)) === (x :: Integer)

-- Find out why this property fails.
--   squareIdentity = square . sqrt
-- sqrt's signature is: sqrt :: Floating a => a -> a
-- In other words, sqrt accepts fixed precision floating point numbers; this leads to a small loss of precision that fails the tests.
-- e.g 0.19999999 /= 0.2

-- Idempotence
-- 1.
  describe "Idempotence." $ do
    it "twice is idempotent (capitalizeWord)" $ do
      property $ \xs -> capitalizeWord xs === twice capitalizeWord (xs :: String)
    it "fourTimes is idempotent (capitalizeWord)" $ do
      property $ \xs -> capitalizeWord xs === fourTimes capitalizeWord (xs :: String)
-- 2.
    it "twice is idempotent (sort)" $ do
      property $ \xs -> sort xs === twice sort (xs :: String)
    it "fourTimes is idempotent (sort)" $ do
      property $ \xs -> sort xs === fourTimes sort (xs :: String)

-- Make a Gen random generator for the datatype

data Fool = Fulse | Frue deriving (Eq, Show)

-- 1. Equal probabilities for each.
sumFoolEqual :: Gen Fool
sumFoolEqual = oneof [return $ Fulse, return $ Frue]

-- 2. 2/3s chance of Fulse, 1/3 chance of Frue.
sumFoolChance = frequency [(2, return $ Fulse), (1, return $ Frue)]