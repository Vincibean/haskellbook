module Ch20.Ch20Spec where

import           Ch20.Ch20
import           Test.Hspec
import           Test.QuickCheck

import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes
import           Data.Foldable                  ( fold )
import           Data.Monoid                    ( Sum(..) )

instance (Arbitrary a) => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance (Arbitrary b) => Arbitrary (Constant' a b) where
  arbitrary = Constant' <$> arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a) => EqProp (Constant a b) where
  (=-=) = eq

instance (Eq b) => EqProp (Constant' a b) where
  (=-=) = eq

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

spec :: Spec
spec = do
  describe "Library Functions" $ do
    it "1. sum'" $ property $ \xs -> sum' xs `shouldBe` sum (xs :: [Int])
    it "2. product'" $ property $ \xs ->
      product' xs `shouldBe` product (xs :: [Int])
    it "3. elem'" $ property $ \x xs ->
      elem' x (x : xs) `shouldBe` elem x (x : xs :: [String])
    it "3. elem''" $ property $ \x xs ->
      elem'' x (x : xs) `shouldBe` elem x (x : xs :: [String])
    it "4. minimum' should work with empty lists"
      $          minimum' ([] :: [Int])
      `shouldBe` Nothing
    it "4. minimum' should work with non-empty lists"
      $ property
      $ \(NonEmpty xs) -> Just (minimum xs) `shouldBe` minimum' (xs :: [Int])
    it "5. maximum' should work with empty lists"
      $          maximum' ([] :: [Int])
      `shouldBe` Nothing
    it "5. maximum' should work with non-empty lists"
      $ property
      $ \(NonEmpty xs) -> Just (maximum xs) `shouldBe` maximum' (xs :: [Int])
    it "6. null' should work with empty lists"
      $          null' ([] :: [Int])
      `shouldBe` True
    it "6. null' should work with non-empty lists"
      $ property
      $ \(NonEmpty xs) -> null' (xs :: [String]) `shouldBe` False
    it "7. length'" $ property $ \xs ->
      length' xs `shouldBe` length' (xs :: [String])
    it "8. toList' should work with Maybe" $ property $ \x ->
      toList' (Just x) `shouldBe` ([x] :: [Bool])
    it "8. toList' should work with Either" $ property $ \x ->
      (toList' (Right x) `shouldBe` ([x] :: [String]))
        >> (toList' (Left x) `shouldBe` ([] :: [String]))
    it "8. toList' should work with tuple" $ property $ \x y ->
      toList' (x :: Double, y :: Int) `shouldBe` [y]
    it "8. toList' should work with List" $ property $ \xs ->
      toList' xs `shouldBe` (xs :: [Integer])
    it "9. fold'" $ property $ \xs -> fold' xs `shouldBe` fold (xs :: [String])
    it "10. foldMap''" $ property $ \xs ->
      foldMap'' Sum xs `shouldBe` foldMap Sum (xs :: [Int])
  describe "Chapter exercises" $ do
    it "1. (Constant)" $ quickBatch $ foldable
      (Constant 12 :: Constant Int (String, Int, [Bool], Int, Double))
    it "1. (Constant')" $ quickBatch $ foldable
      (Constant' ("", 12, [False], 13, 12.2) :: Constant'
          Int
          (String, Int, [Bool], Int, Double)
      )
    it "2." $ quickBatch $ foldable
      (Two "left" ("", 12, [False], 13, 12.2) :: Two
          String
          (String, Int, [Bool], Int, Double)
      )
    it "3." $ quickBatch $ foldable
      (Three True "left" ("", 12, [False], 13, 12.2) :: Three
          Bool
          String
          (String, Int, [Bool], Int, Double)
      )
    it "4." $ quickBatch $ foldable
      (Three' "left" ("", 12, [False], 13, 12.2) ("", 12, [False], 13, 12.2) :: Three'
          String
          (String, Int, [Bool], Int, Double)
      )
    it "5." $ quickBatch $ foldable
      (Four' "left"
             ("", 12, [False], 13, 12.2)
             ("", 12, [False], 13, 12.2)
             ("", 12, [False], 13, 12.2) :: Four'
          String
          (String, Int, [Bool], Int, Double)
      )
  describe "Thinking cap time" $ do
    it "filterF should filter in"
      $ let x :: [Int]
            x = filterF even $ Just 12
        in  x `shouldBe` [12]
    it "filterF' should filter in"
      $ let x :: [Int]
            x = filterF' even $ Just 12
        in  x `shouldBe` [12]
    it "filterF should filter out"
      $ let x :: [Int]
            x = filterF even $ Just 13
        in  x `shouldBe` []
    it "filterF' should filter out"
      $ let x :: [Int]
            x = filterF' even $ Just 13
        in  x `shouldBe` []
    it "filterF should filter in and out"
      $ let x :: [Int]
            x = filterF even [12, 13]
        in  x `shouldBe` [12]
    it "filterF' should filter in and out"
      $ let x :: [Int]
            x = filterF' even [12, 13]
        in  x `shouldBe` [12]

