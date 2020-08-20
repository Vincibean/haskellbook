{-# LANGUAGE FlexibleInstances #-}

module Ch16.Ch16Spec where

import           Ch16.Ch16
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Function

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary (Trivial' a) where
  arbitrary = return $ Trivial' Trivial

instance (Arbitrary a, Arbitrary b) => Arbitrary (Quant a b) where
  arbitrary = oneof [return Finance, Desk <$> arbitrary, Bloor <$> arbitrary]

instance (Arbitrary a) => Arbitrary (K a b) where
  arbitrary = K <$> arbitrary

instance (Arbitrary b) => Arbitrary (Flip K' a b) where
  arbitrary = Flip . K' <$> arbitrary

instance (Arbitrary b) => Arbitrary (EvilGoateeConst a b) where
  arbitrary = GoatyConst <$> arbitrary

instance (Arbitrary1 f, Arbitrary a) => Arbitrary (LiftItOut f a) where
  arbitrary = LiftItOut <$> arbitrary1

instance (Arbitrary1 f, Arbitrary1 g, Arbitrary a) => Arbitrary (Parappa f g a) where
  arbitrary = DaWrappa <$> arbitrary1 <*> arbitrary1

instance (Arbitrary1 f, Arbitrary1 g, Arbitrary a, Arbitrary b) => Arbitrary (IgnoreOne f g a b) where
  arbitrary = IgnoringSomething <$> arbitrary1 <*> arbitrary1

instance (Arbitrary1 g, Arbitrary o, Arbitrary a, Arbitrary t) => Arbitrary (Notorious g o a t) where
  arbitrary = Notorious <$> arbitrary1 <*> arbitrary1 <*> arbitrary1

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = oneof [return Nil, Cons <$> arbitrary <*> arbitrary]

instance (Arbitrary a) => Arbitrary (GoatLord a) where
  arbitrary = oneof
    [ return NoGoat
    , OneGoat <$> arbitrary
    , MoreGoats <$> arbitrary <*> arbitrary <*> arbitrary
    ]

instance (Arbitrary a) => Arbitrary (TalkToMe a) where
  arbitrary =
    oneof [return Halt, Print <$> arbitrary <*> arbitrary, Read <$> arbitrary1]

instance (Show a) => Show (TalkToMe a) where
  show Halt        = "Halt"
  show (Print s a) = "Print" <> s <> show a
  show (Read f   ) = "Read (String -> a)"

-- hack
instance (Eq a) => Eq (TalkToMe a) where
  Halt        == Halt          = True
  (Print s a) == (Print s' a') = s == s' && a == a'
  (Read f   ) == (Read f'    ) = f mempty == f' mempty

spec :: Spec
spec = do
  describe "Heavy Lifting" $ do
    it "1." $ a `shouldBe` [2]
    it "2." $ b `shouldBe` Just ["Hi,lol", "Hellolol"]
    it "3." $ c 1 `shouldBe` -2
    it "4." $ d 0 `shouldBe` "1[0,1,2,3]"
    it "5." $ e `shouldReturn` 3693
  describe "Instances of Func" $ do
    it "1." $ do
      quickCheck (functorIdentity :: Identity Int -> Bool)
      quickCheck
        (functorCompose :: Identity Int -> IntToInt -> IntToInt -> Bool)
    it "2." $ do
      quickCheck (functorIdentity :: Pair Int -> Bool)
      quickCheck (functorCompose :: Pair Int -> IntToInt -> IntToInt -> Bool)
    it "3." $ do
      quickCheck (functorIdentity :: Two String Int -> Bool)
      quickCheck
        (functorCompose :: Two String Int -> IntToInt -> IntToInt -> Bool)
    it "4." $ do
      quickCheck (functorIdentity :: Three String Bool Int -> Bool)
      quickCheck
        (functorCompose :: Three String Bool Int -> IntToInt -> IntToInt -> Bool
        )
    it "5." $ do
      quickCheck (functorIdentity :: Three' Bool Int -> Bool)
      quickCheck
        (functorCompose :: Three' Bool Int -> IntToInt -> IntToInt -> Bool)
    it "6." $ do
      quickCheck (functorIdentity :: Four [Int] String Bool Int -> Bool)
      quickCheck
        (functorCompose :: Four [Int] String Bool Int
          -> IntToInt
          -> IntToInt
          -> Bool
        )
    it "7." $ do
      quickCheck (functorIdentity :: Four' Bool Int -> Bool)
      quickCheck
        (functorCompose :: Four' Bool Int -> IntToInt -> IntToInt -> Bool)
    it "8." $ do
      quickCheck (functorIdentity :: Trivial' Int -> Bool)
      quickCheck
        (functorCompose :: Trivial' Int -> IntToInt -> IntToInt -> Bool)
  describe "Possibly" $ it "should behave as a Functor" $ do
    fmap (+ 1) LolNope `shouldBe` LolNope
    fmap (+ 1) (Yeppers 2) `shouldBe` Yeppers 3
  describe "Short Exercise" $ it "1." $ do
    fmap (+ 1) (First 1) `shouldBe` First 1
    fmap (+ 1) (Second 3) `shouldBe` (Second 4 :: Sum Int Int)
  describe "Chapter Exercises" $ it "3." $ do
    fmap (+ 1) (L 1 2 3) `shouldBe` L 2 2 4
    fmap (+ 1) (R 1 2 3) `shouldBe` R 1 3 3
  describe "Write Functor instances for the following datatypes" $ do
    it "1." $ do
      quickCheck (functorIdentity :: Quant String Int -> Bool)
      quickCheck
        (functorCompose :: Quant String Int -> IntToInt -> IntToInt -> Bool)
    it "2." $ do
      quickCheck (functorIdentity :: K String Int -> Bool)
      quickCheck
        (functorCompose :: K String Int -> IntToInt -> IntToInt -> Bool)
    it "3." $ do
      quickCheck (functorIdentity :: Flip K' String Int -> Bool)
      quickCheck
        (functorCompose :: Flip K' String Int -> IntToInt -> IntToInt -> Bool)
    it "4." $ do
      quickCheck (functorIdentity :: EvilGoateeConst String Int -> Bool)
      quickCheck
        (functorCompose :: EvilGoateeConst String Int
          -> IntToInt
          -> IntToInt
          -> Bool
        )
    it "5." $ do
      quickCheck (functorIdentity :: LiftItOut Maybe Int -> Bool)
      quickCheck
        (functorCompose :: LiftItOut Maybe Int -> IntToInt -> IntToInt -> Bool)
    it "6." $ do
      quickCheck (functorIdentity :: Parappa Maybe Maybe Int -> Bool)
      quickCheck
        (functorCompose :: Parappa Maybe Maybe Int
          -> IntToInt
          -> IntToInt
          -> Bool
        )
    it "7." $ do
      quickCheck (functorIdentity :: IgnoreOne Maybe Maybe String Int -> Bool)
      quickCheck
        (functorCompose :: IgnoreOne Maybe Maybe String Int
          -> IntToInt
          -> IntToInt
          -> Bool
        )
    it "8." $ do
      quickCheck (functorIdentity :: Notorious Maybe Bool String Int -> Bool)
      quickCheck
        (functorCompose :: Notorious Maybe Bool String Int
          -> IntToInt
          -> IntToInt
          -> Bool
        )
    it "9." $ do
      quickCheck (functorIdentity :: List Int -> Bool)
      quickCheck (functorCompose :: List Int -> IntToInt -> IntToInt -> Bool)
    it "10." $ do
      quickCheck (functorIdentity :: GoatLord Int -> Bool)
      quickCheck
        (functorCompose :: GoatLord Int -> IntToInt -> IntToInt -> Bool)
    it "11." $ do
      quickCheck (functorIdentity :: TalkToMe Int -> Bool)
      quickCheck
        (functorCompose :: TalkToMe Int -> IntToInt -> IntToInt -> Bool)
