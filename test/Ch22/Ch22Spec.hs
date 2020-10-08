module Ch22.Ch22Spec where

import           Ch22.Ch22
import           Test.Hspec
import           Test.QuickCheck

import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

import           Data.Tuple
import           Control.Applicative            ( liftA2 )

instance Show (Reader r a) where
  show (Reader _) = "Reader $ a -> b"

instance (Eq a, Monoid r) => Eq (Reader r a) where
  (Reader ra) == (Reader rb) = ra mempty == rb mempty

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Reader a b) where
  arbitrary = Reader <$> arbitrary

instance (Eq a, Eq b, Monoid a) => EqProp (Reader a b) where
  (=-=) = eq

spec :: Spec
spec = do
  describe "Warming up" $ do
    it "composed should give back the right result"
      $          composed "Julie"
      `shouldBe` "EILUJ"
    it "fmapped should give back the right result"
      $          fmapped "Chris"
      `shouldBe` "SIRHC"
    it "composed and fmapped should be isomorphic" $ property $ \s ->
      composed s === fmapped s
    it "tupled should give back the expected result"
      $          tupled "Julie"
      `shouldBe` ("JULIE", "eiluJ")
    it "tupled' should give back the expected result"
      $          tupled' "Julie"
      `shouldBe` ("eiluJ", "JULIE")
    it "tupled' should a swapped tupled" $ property $ \s ->
      tupled s === (swap . tupled') s
  describe "Reading comprehension" $ do
    it "1. myLiftA2 should be isomorphic to liftA2" $ property $ \(Fn f) a b ->
      let act :: Maybe Int
          act = myLiftA2 f (a :: Maybe Int) (b :: Maybe Int)
          exp :: Maybe Int
          exp = liftA2 f a b
      in  act === exp
    it "2. asks should always behave like the underlying function"
      $ property
      $ \(Fn f) a b ->
          let act :: Int
              act = (runReader . asks) f (a :: Int) (b :: Int)
              exp :: Int
              exp = f a b
          in  act === exp
    it "3. Reader should have a valid Applicative"
      $ let trigger = undefined :: Reader [Bool] (String, String, Int)
        in  quickBatch $ applicative trigger
  describe "Reader Monad" $ do
    it "1. Reader should have a valid Monad" 
      $ let trigger = undefined :: Reader [Bool] (String, String, Int)
        in  quickBatch $ monad trigger
    it "2. " $ do
      let dog = runReader getDogRM pers 
      dogsName dog `shouldBe` DogName "Barkley"
      dogsAddress dog `shouldBe` Address "Sesame Street"
  describe "A warm-up stretch" $ do
    it "xs should give back the expected result" $
      xs `shouldBe` Just 6 
    it "ys should give back the expected result" $
      ys `shouldBe` Just 9
    it "zs should give back the expected result" $
      zs `shouldBe` Nothing
    it "z' should give back the expected result" $
      (z' 1 `shouldBe` Just 7) >> (z' 42 `shouldBe` Nothing)
    it "x1 should give back the expected result" $
      x1 `shouldBe` Just (6, 9)
    it "x2 should give back the expected result" $
      x2 `shouldBe` Nothing
    it "x3 should give back the expected result" $
      x3 3  `shouldBe` (Just 9,Just 9)
    it "summed should be uncurry of addition" $
      summed (3, 4)  `shouldBe` 7
    it "bolt should return True if x -> x > 3 && x < 8" $
      (bolt 4 `shouldBe` True) >> (bolt 9 `shouldBe` False)
    it "should provide the same results as the book" $
      (sequenceA [Just 3, Just 2, Just 1] `shouldBe` Just [3,2,1]) >>
      (sequenceA [x, y] `shouldBe` [[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]) >>
      (sequenceA [xs, ys] `shouldBe` Just [6,9]) >>
      (summed <$> ((,) <$> xs <*> ys) `shouldBe` Just 15) >>
      (fmap summed ((,) <$> xs <*> zs) `shouldBe` Nothing) >>
      (bolt 7 `shouldBe` True) >>
      (fmap bolt z `shouldBe` [True,False,False]) >>
      (sequenceA [(>3), (<8), even] 7 `shouldBe` [True, True, False])
    it "wu1 should fold the Boolean conjunction operator over the list of results of sequA" $
      (wu1 7 `shouldBe` False) >> (wu1 4 `shouldBe` True)
    it "wu2 should be the result of applying sequA to s'" $
      wu2 `shouldBe` [True, False, False]
    it "wu3 should be the result of applying bolt to ys" $
      wu3 `shouldBe` False






