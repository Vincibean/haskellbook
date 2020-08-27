module Ch17.Ch17Spec where

import           Ch17.Ch17
import           Test.Hspec
import           Data.Monoid

import           Test.QuickCheck         hiding ( Failure
                                                , Success
                                                )
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

import           Control.Applicative            ( liftA3 )

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = oneof [pure Nil, Cons <$> arbitrary <*> arbitrary]

instance (Arbitrary a) => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = oneof [Failure <$> arbitrary, Success <$> arbitrary]

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = liftA3 Three' arbitrary arbitrary arbitrary

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary



instance (Eq a) => EqProp (List a) where
  (=-=) = eq

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
   where
    xs' = let (ZipList' l) = xs in take' 3000 l
    ys' = let (ZipList' l) = ys in take' 3000 l

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq

instance (Eq a) => EqProp (Pair a) where
  (=-=) = eq

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

data Errors = DividedByZero | StackOverflow | MooglesChewedWires deriving (Eq, Show)

spec :: Spec
spec = do
  describe "Identity Instance" $ it "should be a valid(ish) Applicative" $ do
    (+ 3) <$> Identity 5 `shouldBe` Identity 8
    (+) <$> Identity 3 <*> Identity 5 `shouldBe` Identity 8
  describe "Constant Instance" $ it "should be a valid(ish) Applicative" $ do
    getConstant (pure 1 :: Constant String Int) `shouldBe` ""
    Constant (Sum 1)
      <*>        Constant (Sum 2)
      `shouldBe` (Constant (Sum 3) :: Constant (Sum Int) String)
  describe "List Applicative" $ do
    it "should be a valid Applicative" $ quickBatch $ applicative
      (Cons ("b", "w", 1) Nil :: List (String, String, Int))
    it "should be a valid Applicative using the book's example" $ do
      let functions = Cons (+ 1) (Cons (* 2) Nil)
      let values    = Cons 1 (Cons 2 Nil)
      functions <*> values `shouldBe` Cons 2 (Cons 3 (Cons 2 (Cons 4 Nil)))
  describe "ZipList' Applicative"
    $ it "should be a valid Applicative"
    $ quickBatch
    $ applicative
        (ZipList' (Cons ("b", "w", 1) Nil) :: ZipList' (String, String, Int))
  describe "Variations on Either" $ do
    it "should be a valid Applicative" $ quickBatch $ applicative
      (Success ("b", "w", 1) :: Validation [String] (String, String, Int))
    it "should be a valid Applicative using the book's example" $ do
      let success = Success (+ 1) <*> Success 1
      success `shouldBe` (Success 2 :: Validation [Errors] Int)
      let failure = Success (+ 1) <*> Failure [StackOverflow]
      failure `shouldBe` Failure [StackOverflow]
      let failure' = Failure [StackOverflow] <*> Success (+ 1)
      failure' `shouldBe` (Failure [StackOverflow] :: Validation [Errors] Int)
      let failures =
            Failure [MooglesChewedWires]
              <*> (Failure [StackOverflow] :: Validation [Errors] Int)
      failures
        `shouldBe` (Failure [MooglesChewedWires, StackOverflow] :: Validation
                       [Errors]
                       Int
                   )
  describe "Chapter exercises" $ do
    it "Pair's Applicative should be a valid Applicative"
      $ quickBatch
      $ applicative
          (Pair ("b", "w", 1) ("b", "w", 1) :: Pair (String, String, Int))
    it "Two's Applicative should be a valid Applicative"
      $ quickBatch
      $ applicative (Two "" ("b", "w", 1) :: Two String (String, String, Int))
    it "Three's Applicative should be a valid Applicative"
      $ quickBatch
      $ applicative
          (Three "" [] ("b", "w", 1) :: Three String [Int] (String, String, Int)
          )
    it "The instance of Applicative of Three' should be a valid Applicative"
      $ quickBatch
      $ applicative
          (Three' "" ("b", "w", 1) ("b", "w", 1) :: Three'
              String
              (String, String, Int)
          )
    it "Four's Applicative should be a valid Applicative"
      $ quickBatch
      $ applicative
          (Four "" [] [""] ("b", "w", 1) :: Four
              String
              [Int]
              [String]
              (String, String, Int)
          )
    it "The instance of Applicative of Four' should be a valid Applicative"
      $ quickBatch
      $ applicative
          (Four' "" "" "" ("b", "w", 1) :: Four' String (String, String, Int))
  describe "Combinations"
    $          it "should give back the same results as fold"
    $          combos stops vowels stops
    `shouldBe` [ (s1, v, s2) | s1 <- stops, v <- vowels, s2 <- stops ]
