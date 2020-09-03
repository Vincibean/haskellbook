module Ch18.Ch18Spec where

import           Ch18.Ch18
import           Test.Hspec
import           Test.QuickCheck

import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes hiding ( bind )

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = oneof [First <$> arbitrary, Second <$> arbitrary]

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither b a) where
  arbitrary = oneof [Left' <$> arbitrary, Right' <$> arbitrary]

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = oneof [return Nil, Cons <$> arbitrary <*> arbitrary]

instance (Eq a, Eq b) => EqProp (Sum a b) where
  (=-=) = eq

instance (Eq a) => EqProp (Nope a) where
  (=-=) = eq

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where
  (=-=) = eq

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

spec :: Spec
spec = do
  describe "The answer is the exercise"
    $ it "should be a valid(ish) bind"
    $ property
    $ \xs (Fn f) -> bind (f :: Int -> [Int]) (xs :: [Int]) === (xs >>= f)
  describe "Either Monad" $ do
    it "should be a valid functor" $ quickBatch $ functor
      (Second ("b", "w", 1) :: Sum [Bool] (String, String, Int))
    it "should be a valid applicative" $ quickBatch $ applicative
      (Second ("b", "w", 1) :: Sum [Bool] (String, String, Int))
    it "should be a valid monad" $ quickBatch $ monad
      (Second ("b", "w", 1) :: Sum [Bool] (String, String, Int))
  describe "Chapter Exercises" $ do
    it "1." $ quickBatch $ monad (NopeDotJpg :: Nope (Bool, String, Int))
    it "2." $ quickBatch $ monad
      (Left' ("b", "w", 1) :: PhhhbbtttEither String (String, String, Int))
    it "3." $ quickBatch $ monad $ Identity ("b", "w", True)
    it "4." $ quickBatch $ monad
      (Cons ("b", "w", True) Nil :: List (String, String, Bool))
  describe
      "Write the following functions using the methods provided by Monad and Functor"
    $ it "1."
    $ do
        j [[1, 2], [], [3]] `shouldBe` [1, 2, 3]
        j (Just (Just 1)) `shouldBe` Just 1
        j (Just Nothing) `shouldBe` (Nothing :: Maybe (Maybe Int))
        j Nothing `shouldBe` (Nothing :: Maybe (Maybe Int))
