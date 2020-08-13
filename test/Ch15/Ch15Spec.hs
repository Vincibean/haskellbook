module Ch15.Ch15Spec where

import           Ch15.Ch15
import           Test.Hspec
import           Data.Semigroup
import           Test.QuickCheck         hiding ( Failure
                                                , Success
                                                )

instance (Arbitrary a) => Arbitrary (Optional a) where
  arbitrary = do
    a <- arbitrary
    elements [Nada, Only a]

instance (Arbitrary a) => Arbitrary (First' a) where
  arbitrary = First' <$> arbitrary

instance Arbitrary Trivial where
  arbitrary = return Trivial

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    Three a b <$> arbitrary

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    Four a b c <$> arbitrary

instance Arbitrary BoolConj where
  arbitrary = BoolConj <$> arbitrary

instance Arbitrary BoolDisj where
  arbitrary = BoolDisj <$> arbitrary

-- https://hackage.haskell.org/package/QuickCheck-2.14.1/docs/src/Test.QuickCheck.Arbitrary.html#line-411
instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = Combine <$> arbitrary1

-- hack
instance Show (Combine a b) where
  show c = "Combine { a -> b }"

-- hack
instance (Monoid a, Eq b) => Eq (Combine a b) where
  (Combine f1) == (Combine f2) = f1 mempty == f2 mempty

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = Comp <$> arbitrary1

-- hack
instance Show (Comp a) where
  show c = "Comp { a -> a }"

-- hack
instance (Monoid a, Eq a) => Eq (Comp a) where
  (Comp f1) == (Comp f2) = f1 mempty == f2 mempty


spec :: Spec
spec = do
  describe "Optional Monoid" $ do
    it "Only (Sum 1) `mappend` Only (Sum 1)"
      $          Only (Sum 1)
      `mappend`  Only (Sum 1)
      `shouldBe` Only (Sum 2)
    it "Only (Product 4) `mappend` Only (Product 2)"
      $          Only (Product 4)
      `mappend`  Only (Product 2)
      `shouldBe` Only (Product 8)
    it "Only (Sum 1) `mappend` Nada"
      $          Only (Sum 1)
      `mappend`  Nada
      `shouldBe` Only (Sum 1)
    it "Only [1] `mappend` Nada" $ Only [1] `mappend` Nada `shouldBe` Only [1]
    it "Nada `mappend` Only (Sum 1)"
      $          Nada
      `mappend`  Only (Sum 1)
      `shouldBe` Only (Sum 1)
  describe "Madness"
    $ it "madlibbinBetter' should give the expected result"
    $ madlibbinBetter' "gosh" "slowly" "ball" "annoying"
    `shouldBe` "gosh! he said slowly as he jumped into his car ball and drove off with his annoying wife."
  describe "Maybe Another Monoid" $ do
    it "monoidAssoc" $ property $ \f1 f2 f3 ->
      monoidAssoc f1 f2 (f3 :: First' String)
    it "monoidLeftIdentity" $ property $ \f ->
      monoidLeftIdentity (f :: First' String)
    it "monoidRightIdentity" $ property $ \f ->
      monoidRightIdentity (f :: First' String)
    it "First' (Only 1) `mappend` First' Nada"
      $          First' (Only 1)
      `mappend`  First' Nada
      `shouldBe` First' (Only 1)
    it "First' Nada `mappend` First' Nada" $ do
      let res = First' Nada `mappend` First' Nada
      res `shouldBe` (First' Nada :: First' Int)
    it "First' Nada `mappend` First' (Only 2)"
      $          First' Nada
      `mappend`  First' (Only 2)
      `shouldBe` First' (Only 2)
    it "First' (Only 1) `mappend` First' (Only 2)"
      $          First' (Only 1)
      `mappend`  First' (Only 2)
      `shouldBe` First' (Only 1)
  describe "Semigroup exercises" $ do
    it "1." $ property $ \f1 f2 f3 -> semigroupAssoc f1 f2 (f3 :: Trivial)
    it "2." $ property $ \f1 f2 f3 ->
      semigroupAssoc f1 f2 (f3 :: Identity String)
    it "3." $ property $ \f1 f2 f3 ->
      semigroupAssoc f1 f2 (f3 :: Two String [Int])
    it "4." $ property $ \f1 f2 f3 ->
      semigroupAssoc f1 f2 (f3 :: Three String [Int] [Bool])
    it "5." $ property $ \f1 f2 f3 ->
      semigroupAssoc f1 f2 (f3 :: Four String [Int] [Bool] String)
    it "6.a" $ BoolConj True <> BoolConj True `shouldBe` BoolConj True
    it "6.b" $ BoolConj True <> BoolConj False `shouldBe` BoolConj False
    it "7.a" $ BoolDisj True <> BoolDisj True `shouldBe` BoolDisj True
    it "7.b" $ BoolDisj True <> BoolDisj False `shouldBe` BoolDisj True
    it "8.a" $ Fst 1 <> Snd 2 `shouldBe` Snd 2
    it "8.b" $ Fst 1 <> Fst 2 `shouldBe` (Fst 2 :: Or Int Int)
    it "8.c" $ Snd 1 <> Fst 2 `shouldBe` Snd 1
    it "8.d" $ Snd 1 <> Snd 2 `shouldBe` (Snd 1 :: Or Int Int)
    it "9.a" $ do
      let f   = Combine $ \n -> Sum (n + 1)
      let g   = Combine $ \n -> Sum (n - 1)
      let res = getSum . unCombine (f <> g) $ 0
      res `shouldBe` 0
    it "9.b" $ do
      let f   = Combine $ \n -> Sum (n + 1)
      let g   = Combine $ \n -> Sum (n - 1)
      let res = getSum . unCombine (f <> g) $ 1
      res `shouldBe` 2
    it "9.c" $ do
      let f   = Combine $ \n -> Sum (n + 1)
      let res = getSum . unCombine (f <> f) $ 1
      res `shouldBe` 4
    it "9.d" $ do
      let f   = Combine $ \n -> Sum (n + 1)
      let g   = Combine $ \n -> Sum (n - 1)
      let res = getSum . unCombine (g <> f) $ 1
      res `shouldBe` 2
    it "9.e" $ property $ \(Fn f) (Fn g) x ->
      let cf  = Combine (f :: Integer -> String)
          cg  = Combine (g :: Integer -> String)
          exp = f x <> g x
          act = unCombine (cf <> cg) x
      in  act `shouldBe` exp
    it "10" $ property $ \(Fn f) (Fn g) x ->
      let cf  = Comp (f :: String -> String)
          cg  = Comp (g :: String -> String)
          exp = f . g $ x
          act = unComp (cf <> cg) x
      in  act `shouldBe` exp
    it "11.a"
      $          Failure "1"
      <>         Failure "2"
      `shouldBe` (Failure "12" :: Validation String String)
    it "11.b"
      $          Success "1"
      <>         Success "2"
      `shouldBe` (Success "1" :: Validation String String)
    it "11.c"
      $          Failure "1"
      <>         Success "2"
      `shouldBe` (Failure "1" :: Validation String String)
    it "11.d"
      $          Success "1"
      <>         Failure "2"
      `shouldBe` (Failure "2" :: Validation String String)
    it "12.a"
      $          AccumulateRight (Failure "1")
      <>         AccumulateRight (Failure "2")
      `shouldBe` AccumulateRight (Failure "1" :: Validation String String)
    it "12.b"
      $          AccumulateRight (Success "1")
      <>         AccumulateRight (Success "2")
      `shouldBe` AccumulateRight (Success "12" :: Validation String String)
    it "12.c"
      $          AccumulateRight (Failure "1")
      <>         AccumulateRight (Success "2")
      `shouldBe` AccumulateRight (Failure "1" :: Validation String String)
    it "12.d"
      $          AccumulateRight (Success "1")
      <>         AccumulateRight (Failure "2")
      `shouldBe` AccumulateRight (Failure "2" :: Validation String String)
    it "13.a"
      $          AccumulateBoth (Failure "1")
      <>         AccumulateBoth (Failure "2")
      `shouldBe` AccumulateBoth (Failure "12" :: Validation String String)
    it "13.b"
      $          AccumulateBoth (Success "1")
      <>         AccumulateBoth (Success "2")
      `shouldBe` AccumulateBoth (Success "12" :: Validation String String)
    it "13.c"
      $          AccumulateBoth (Failure "1")
      <>         AccumulateBoth (Success "2")
      `shouldBe` AccumulateBoth (Failure "1" :: Validation String String)
    it "13.d"
      $          AccumulateBoth (Success "1")
      <>         AccumulateBoth (Failure "2")
      `shouldBe` AccumulateBoth (Failure "2" :: Validation String String)
  describe "Monoid exercises" $ do
    it "1. monoidAssoc" $ property $ \f1 f2 f3 ->
      monoidAssoc f1 f2 (f3 :: Trivial)
    it "1. monoidAssoc (w quickcheck)"
      $ quickCheck (monoidAssoc :: TrivialAssoc)
    it "1. monoidLeftIdentity" $ property $ \f ->
      monoidLeftIdentity (f :: Trivial)
    it "1. monoidRightIdentity" $ property $ \f ->
      monoidRightIdentity (f :: Trivial)
    it "2. monoidAssoc" $ property $ \f1 f2 f3 ->
      monoidAssoc f1 f2 (f3 :: Identity String)
    it "2. monoidLeftIdentity" $ property $ \f ->
      monoidLeftIdentity (f :: Identity String)
    it "2. monoidRightIdentity" $ property $ \f ->
      monoidRightIdentity (f :: Identity String)
    it "3. monoidAssoc" $ property $ \f1 f2 f3 ->
      monoidAssoc f1 f2 (f3 :: Two String [Int])
    it "3. monoidLeftIdentity" $ property $ \f ->
      monoidLeftIdentity (f :: Two String [Int])
    it "3. monoidRightIdentity" $ property $ \f ->
      monoidRightIdentity (f :: Two String [Int])
    it "4. monoidAssoc" $ property $ \f1 f2 f3 ->
      monoidAssoc f1 f2 (f3 :: BoolConj)
    it "4. monoidLeftIdentity" $ property $ \f ->
      monoidLeftIdentity (f :: BoolConj)
    it "4. monoidRightIdentity" $ property $ \f ->
      monoidRightIdentity (f :: BoolConj)
    it "4. (BoolConj True) `mappend` mempty" $ do
      let res = BoolConj True `mappend` mempty
      res `shouldBe` BoolConj True
    it "4. mempty `mappend` (BoolConj False)" $ do
      let res = mempty `mappend` BoolConj False
      res `shouldBe` BoolConj False
    it "5. monoidAssoc" $ property $ \f1 f2 f3 ->
      monoidAssoc f1 f2 (f3 :: BoolDisj)
    it "5. monoidLeftIdentity" $ property $ \f ->
      monoidLeftIdentity (f :: BoolDisj)
    it "5. monoidRightIdentity" $ property $ \f ->
      monoidRightIdentity (f :: BoolDisj)
    it "5. (BoolDisj True) `mappend` mempty" $ do
      let res = BoolDisj True `mappend` mempty
      res `shouldBe` BoolDisj True
    it "5. mempty `mappend` (BoolDisj False)" $ do
      let res = mempty `mappend` BoolDisj False
      res `shouldBe` BoolDisj False
    it "6. monoidAssoc" $ property $ \f1 f2 f3 ->
      monoidAssoc f1 f2 (f3 :: Combine String [Int])
    it "6. monoidLeftIdentity" $ property $ \f ->
      monoidLeftIdentity (f :: Combine String [Int])
    it "6. monoidRightIdentity" $ property $ \f ->
      monoidRightIdentity (f :: Combine String [Int])
    it "6. (BoolDisj True) `mappend` mempty" $ do
      let f   = Combine $ \n -> Sum (n + 1)
      let res = getSum . unCombine (mappend f mempty) $ 1
      res `shouldBe` 2
    it "7. monoidAssoc" $ property $ \f1 f2 f3 ->
      monoidAssoc f1 f2 (f3 :: Comp String)
    it "7. monoidLeftIdentity" $ property $ \f ->
      monoidLeftIdentity (f :: Comp String)
    it "7. monoidRightIdentity" $ property $ \f ->
      monoidRightIdentity (f :: Comp String)
    it "8.1" $ do
      let f'  = Mem $ \s -> ("hi", s + 1)
      let res = runMem (f' <> mempty) (0 :: Sum Int)
      res `shouldBe` ("hi", 1)
    it "8.2" $ do
      let f'  = Mem $ \s -> ("hi", s + 1)
      let res = runMem (mempty <> f') (0 :: Sum Int)
      res `shouldBe` ("hi", 1)
    it "8.3" $ do
      let f'  = Mem $ \s -> ("hi", s + 1)
      let res = runMem mempty 0 :: (String, Sum Int)
      res `shouldBe` ("", 0)
    it "8.4" $ do
      let f'  = Mem $ \s -> ("hi", s + 1)
      let res = runMem (f' <> mempty) 0 == runMem f' (0 :: Sum Int)
      res `shouldBe` True
    it "8.5" $ do
      let f'  = Mem $ \s -> ("hi", s + 1)
      let res = runMem (mempty <> f') 0 == runMem f' (0 :: Sum Int)
      res `shouldBe` True



monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool
