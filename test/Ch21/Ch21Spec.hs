module Ch21.Ch21Spec where

import           Ch21.Ch21
import           Test.Hspec
import           Test.QuickCheck

import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance (Arbitrary a) => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance (Arbitrary a) => Arbitrary (Optional a) where
  arbitrary = oneof [pure Nada, Yep <$> arbitrary]

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = oneof [pure Nil, Cons <$> arbitrary <*> arbitrary]

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = Big <$> arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = Bigger <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary a, Arbitrary (n a)) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance (Arbitrary a) => Arbitrary (Tree a) where
  arbitrary = oneof
    [ pure Empty
    , Leaf <$> arbitrary
    , Node <$> arbitrary <*> arbitrary <*> arbitrary
    ]

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

instance (Eq a) => EqProp (Constant a b) where
  (=-=) = eq

instance (Eq a) => EqProp (Optional a) where
  (=-=) = eq

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

instance (Eq a, Eq b) => EqProp (Pair a b) where
  (=-=) = eq

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq

instance (Eq a, Eq b) => EqProp (Bigger a b) where
  (=-=) = eq

instance (Eq a, Eq (n a)) => EqProp (S n a) where
  (=-=) = eq

instance (Eq a) => EqProp (Tree a) where
  (=-=) = eq

spec :: Spec
spec = describe "Chapter Exercises" $ do
  it "1. Identity"
    $ let trigger = undefined :: Identity (Int, Int, [Int])
      in  quickBatch $ traversable trigger
  it "2. Constant"
    $ let trigger = undefined :: Constant String (Int, Int, [Int])
      in  quickBatch $ traversable trigger
  it "3. Maybe"
    $ let trigger = undefined :: Optional (Int, Int, [Int])
      in  quickBatch $ traversable trigger
  it "4. List"
    $ let trigger = undefined :: List (Int, Int, [Int])
      in  quickBatch $ traversable trigger
  it "5. Three"
    $ let trigger = undefined :: Three Bool String (Int, Int, [Int])
      in  quickBatch $ traversable trigger
  it "6. Pair"
    $ let trigger = undefined :: Pair Bool (Int, Int, [Int])
      in  quickBatch $ traversable trigger
  it "7. Big"
    $ let trigger = undefined :: Big Bool (Int, Int, [Int])
      in  quickBatch $ traversable trigger
  it "8. Bigger"
    $ let trigger = undefined :: Bigger Bool (Int, Int, [Int])
      in  quickBatch $ traversable trigger
  it "9. S"
    $ let trigger = undefined :: S Maybe (Int, Int, [Int])
      in  quickBatch $ traversable trigger
  it "10. Instances for Tree: Foldable"
    $ let trigger = undefined :: S Tree (String, Int, [Bool], Int, Double)
      in  quickBatch $ foldable trigger
  it "10. Instances for Tree: Traversable"
    $ let trigger = undefined :: S Tree (Int, Int, [Int])
      in  quickBatch $ traversable trigger
