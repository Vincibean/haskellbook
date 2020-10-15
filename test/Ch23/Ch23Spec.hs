{-# LANGUAGE TupleSections #-}

module Ch23.Ch23Spec where

import           Ch23.Ch23
import           Test.Hspec
import           Test.QuickCheck

import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

import           System.Random

-- hack
instance Show (Moi s a) where
  show _ = "Moi { s -> (a, s) }"

-- hack
instance (Monoid s, Eq a, Eq s) => Eq (Moi s a) where
  (Moi f1) == (Moi f2) = f1 mempty == f2 mempty

instance (Arbitrary a, Arbitrary s, CoArbitrary s) => Arbitrary (Moi s a) where
  arbitrary = Moi <$> arbitrary

instance (Eq a, Eq s, Monoid s) => EqProp (Moi s a) where
  (=-=) = eq

spec :: Spec
spec = do
  describe "Roll your own" $ do
    it "1."
      $ let seed = mkStdGen 0
        in  rollsToGetN 20 seed `shouldBe` rollsToGetTwenty seed
    it "2." $ do
      let seed            = mkStdGen 0
      let (counts, rolls) = rollsCountLogged 20 seed
      counts `shouldBe` rollsToGetTwenty seed
      rolls `shouldBe` [DieSix, DieSix, DieFour, DieOne, DieFive]
  describe "Implement the Functor instance for State" $ do
    it "works in the given example"
      $ let f   = (+ 1) <$> Moi (0, )
            res = runMoi f 0
        in  res `shouldBe` (1, 0)
    it "has a valid functor instance"
      $ let trigger = undefined :: Moi [Int] (Int, Int, Int)
        in  quickBatch $ functor trigger
  describe "Write the Applicative instance for State"
    $ it "has a valid applicative instance"
    $ let trigger = undefined :: Moi String (String, String, Int)
      in  quickBatch $ applicative trigger
  describe "Write the Monad instance for State"
    $ it "has a valid monad instance"
    $ let trigger = undefined :: Moi String (String, String, Int)
      in  quickBatch $ monad trigger
  describe "FizzBuzz differently"
    $          it "provides a reversed FizzBuzz"
    $          fizzbuzzFromTo 1 10
    `shouldBe` ["1", "2", "Fizz", "4", "Buzz", "Fizz", "7", "8", "Fizz", "Buzz"]
  describe "Chapter exercises" $ do
    it "1."
      $          runMoi get' "curryIsAmaze"
      `shouldBe` ("curryIsAmaze", "curryIsAmaze")
    it "2." $ runMoi (put' "blah") "woot" `shouldBe` ((), "blah")
    it "3." $ do
      exec' (put' "wilma") "daphne" `shouldBe` "wilma"
      exec' get' "scooby papu" `shouldBe` "scooby papu"
    it "4." $ do
      eval' get' "bunnicula" `shouldBe` "bunnicula"
      eval' get' "stake a bunny" `shouldBe` "stake a bunny"
    it "5. modify'" $ do
      let f = modify' (+ 1)
      runMoi f 0 `shouldBe` ((), 1)
      runMoi (f >> f) 0 `shouldBe` ((), 2)
    it "5. modify''" $ do
      let f = modify'' (+ 1)
      runMoi f 0 `shouldBe` ((), 1)
      runMoi (f >> f) 0 `shouldBe` ((), 2)
    it "5. modify'''" $ do
      let f = modify''' (+ 1)
      runMoi f 0 `shouldBe` ((), 1)
      runMoi (f >> f) 0 `shouldBe` ((), 2)
