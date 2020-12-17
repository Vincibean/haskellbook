module Ch30.Ch30Spec where

import Ch30.Ch30 ( canICatch, tryS )
import Test.Hspec ( describe, it, shouldBe, Spec )
import Control.Exception
    ( throwIO,
      ArithException(DivideByZero),
      AsyncException(StackOverflow) )
import System.Exit ( ExitCode(ExitSuccess) )

spec :: Spec
spec = do
  describe "canICatch" $ do
    it "catches DivideByZero" $ do
      err <- canICatch DivideByZero
      err `shouldBe` Left DivideByZero
    it "catches StackOverflow" $ do
      err <- canICatch StackOverflow
      err `shouldBe` Left StackOverflow
    it "catches ExitSuccess" $ do
      err <- canICatch ExitSuccess
      err `shouldBe` Left ExitSuccess
  describe "tryS" $ do
    it "catches DivideByZero" $ do
      err <- tryS $ throwIO DivideByZero
      err `shouldBe` Left DivideByZero
    it "catches StackOverflow" $ do
      err <- tryS $ throwIO StackOverflow
      err `shouldBe` Left StackOverflow
    it "catches ExitSuccess" $ do
      err <- tryS $ throwIO ExitSuccess
      err `shouldBe` Left ExitSuccess