module Ch25.Ch25Spec where

import           Ch25.Ch25
import           Data.Bifunctor
import           Test.Hspec
import           Test.QuickCheck

import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

instance (Arbitrary (f (g a))) => Arbitrary (Compose f g a) where
  arbitrary = Compose <$> arbitrary

instance (Eq (f (g a))) => EqProp (Compose f g a) where
  (=-=) = eq

spec :: Spec
spec = do
  describe "GOTCHA! Exercise time"
    $ it "has a valid Applicative instance"
    $ let trigger = undefined :: Compose Maybe [] (String, String, Int)
      in  quickBatch $ applicative trigger
  describe "Compose instances" $ do
    it "has a valid Foldable instance"
      $ let trigger =
              undefined :: Compose Maybe [] (String, Int, [Bool], Int, Double)
        in  quickBatch $ foldable trigger
    it "has a valid Traversable instance"
      $ let trigger = undefined :: Compose Maybe [] (Int, Int, [Int])
        in  quickBatch $ traversable trigger
  describe "And now for something completely different" $ do
    it "Deux has a valid (I hope) Bifunctor instance"
      $          bimap (<> "left") (<> "right") (Deux "I am " "I am ")
      `shouldBe` Deux "I am left" "I am right"
    it "Const has a valid (I hope) Bifunctor instance"
      $          bimap (<> "left") (<> "right") (Const "I am ")
      `shouldBe` Const "I am left"
    it "Drei has a valid (I hope) Bifunctor instance"
      $          bimap (<> "left") (<> "right") (Drei "I am " "I am " "I am ")
      `shouldBe` Drei "I am " "I am left" "I am right"
    it "SuperDrei has a valid (I hope) Bifunctor instance"
      $          bimap (<> "left") (<> "right") (SuperDrei "I am " "I am ")
      `shouldBe` SuperDrei "I am " "I am left"
    it "SuperDrei' has a valid (I hope) Bifunctor instance"
      $ let s1 :: SuperDrei String String Int
            s1 = SuperDrei "I am " "I am "
            s2 :: SuperDrei String String Int
            s2 = SuperDrei "I am left" "I am right"
        in  bimap (<> "left") (<> "right") (SuperDrei' s1)
              `shouldBe` SuperDrei' s2
    it "SemiDrei has a valid (I hope) Bifunctor instance"
      $          bimap (<> "left") (<> "right") (SemiDrei "I am ")
      `shouldBe` SemiDrei "I am "
    it "Quadriceps has a valid (I hope) Bifunctor instance"
      $          bimap (<> "left") (<> "right") (Quadzzz 1 True "I am " "I am ")
      `shouldBe` Quadzzz 1 True "I am left" "I am right"
    it "Either has a valid (I hope) Bifunctor instance" $ do
      let r :: Either' String String
          r = Right' "I am "
      let l :: Either' String String
          l = Left' "I am "
      let f = bimap (<> "left") (<> "right")
      f r `shouldBe` Right' "I am right"
      f l `shouldBe` Left' "I am left"
