module Ch26.Ch26Spec where

import Ch26.Ch26

import           Test.Hspec
import           Test.QuickCheck

import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

import Data.Functor.Identity
import Data.Functor.Classes
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

instance (Arbitrary1 m, Arbitrary e, Arbitrary a) => Arbitrary (EitherT e m a) where
  arbitrary = EitherT <$> arbitrary1

instance (Eq e, Eq1 m, Eq a) => EqProp (EitherT e m a) where
  (=-=) = eq

spec :: Spec
spec = do
  describe "EitherT" $ do
    it "1. Write the Functor instance for EitherT" $ 
      let trigger = undefined :: EitherT [Bool] Identity (String, String, Int)
      in  quickBatch $ functor trigger
    it "2. Write the Applicative instance for EitherT" $ 
      let trigger = undefined :: EitherT [Bool] Identity (String, String, Int)
      in  quickBatch $ applicative trigger
    it "3. Write the Monad instance for EitherT" $ 
      let trigger = undefined :: EitherT [Bool] Identity (String, String, Int)
      in  quickBatch $ monad trigger
    it "4. Write the swapEitherT helper function for EitherT" $ 
      property $ \e ->  let swapped :: EitherT [Bool] Identity (String, String, Int)
                            swapped = swapEitherT e 
                            exp :: Either (String, String, Int) [Bool]
                            exp = (runIdentity . runEitherT) e
                            act = (runIdentity . runEitherT) swapped
                        in case (act, exp) of
                          (Left x, Right x') -> x `shouldBe` x'
                          (Right x, Left x') -> x `shouldBe` x'
                          _ -> expectationFailure "They should be swapped!"
    it "5. Write the transformer variant of the either catamorphism" $ 
      property $ \e lf rf -> let lmf :: [Bool] -> Identity String
                                 lmf = pure . lf
                                 rmf :: (String, String, Int) -> Identity String
                                 rmf = pure . rf
                                 et :: EitherT [Bool] Identity (String, String, Int)
                                 et = EitherT $ Identity e
                                 exp = either lf rf e
                                 act = runIdentity $ eitherT lmf rmf et
                             in act `shouldBe` exp
  describe "Write the code" $ do
    it "1. 2. rDec" $ 
      fmap (runReader rDec) [1..10] `shouldBe` [0..9]
    it "3. 4. rShow" $ 
      fmap (runReader rShow) [1..10] `shouldBe` ["1","2","3","4","5","6","7","8","9","10"]
    it "5. rPrintAndInc" $ do
      result <- runReaderT rPrintAndInc 1
      result `shouldBe` 2
      result' <- traverse (runReaderT rPrintAndInc) [1..10]
      result' `shouldBe` [2..11]
    it "6. sPrintIncAccum" $ do
      result <- runStateT sPrintIncAccum 10
      result `shouldBe` ("10",11)
      result' <-  mapM (runStateT sPrintIncAccum) [1..5]
      result' `shouldBe` [("1",2),("2",3),("3",4),("4",5),("5",6)]
      