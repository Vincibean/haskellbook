{-# LANGUAGE TupleSections #-}

module Ch15.Ch15 where

-- Optional Monoid
-- Write the Monoid instance for our Maybe type renamed to Optional.

data Optional a = Nada | Only a deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  (Only a1) <> (Only a2) = Only $ a1 <> a2
  opt       <> Nada      = opt
  Nada      <> opt       = opt

instance Monoid a => Monoid (Optional a) where
  mempty = Nada

-- Madness

type Verb = String

type Adjective = String

type Adverb = String

type Noun = String

type Exclamation = String

madlibbinBetter' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbinBetter' e adv noun adj = mconcat
  [ e
  , "! he said "
  , adv
  , " as he jumped into his car "
  , noun
  , " and drove off with his "
  , adj
  , " wife."
  ]

-- Maybe Another Monoid
-- Write a Monoid instance for Maybe type which doesn’t require a Monoid for the contents.
newtype First' a = First' { getFirst' :: Optional a } deriving (Eq, Show)

instance Semigroup (First' a) where
  (<>) = firstMappend


instance Monoid (First' a) where
  mempty = First' Nada

firstMappend :: First' a -> First' a -> First' a
firstMappend f1 f2 = case getFirst' f1 of
  (Only _) -> f1
  Nada     -> f2

type FirstMappend = First' String -> First' String -> First' String -> Bool

type FstId = First' String -> Bool

-- Semigroup exercises
-- 1.

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

-- 2.
newtype Identity a = Identity a deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Identity a) where
  (Identity i1) <> (Identity i2) = Identity $ i1 <> i2

-- 3. 
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a1 b1) <> (Two a2 b2) = Two (a1 <> a2) (b1 <> b2)

-- 4. 
data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three a1 b1 c1) <> (Three a2 b2 c2) = Three (a1 <> a2) (b1 <> b2) (c1 <> c2)

-- 5. 
data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (Four a1 b1 c1 d1) <> (Four a2 b2 c2 d2) =
    Four (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2)

-- 6.
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj b1) <> (BoolConj b2) = BoolConj $ b1 && b2

-- 7.
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj b1) <> (BoolDisj b2) = BoolDisj $ b1 || b2

-- 8. 
data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
  (  Fst a) <> f = f
  f@(Snd a) <> _ = f

-- 9.
newtype Combine a b = Combine { unCombine :: a -> b }

-- instance (Semigroup b) => Semigroup (Combine a b) where
--   c1 <> c2 =
--     let f1 = unCombine c1
--         f2 = unCombine c2
--     in  Combine (\a -> f1 a <> f2 a)

instance (Semigroup b) => Semigroup (Combine a b) where
  Combine f1 <> Combine f2 = Combine $ f1 <> f2

-- 10.
newtype Comp a = Comp { unComp :: a -> a }

instance Semigroup (Comp a) where
  Comp f1 <> Comp f2 = Comp $ f1 . f2

-- 11.
data Validation a b = Failure a | Success b deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  Failure a <> Failure b     = Failure $ a <> b
  _         <> f@(Failure a) = f
  x         <> _             = x

-- 12.
newtype AccumulateRight a b = AccumulateRight (Validation a b) deriving (Eq, Show)

instance Semigroup b => Semigroup (AccumulateRight a b) where
  f@(AccumulateRight (Failure a)) <> _                               = f
  _                               <> f@(AccumulateRight (Failure a)) = f
  (AccumulateRight (Success a1)) <> (AccumulateRight (Success a2)) =
    AccumulateRight . Success $ a1 <> a2

-- 13.
newtype AccumulateBoth a b = AccumulateBoth (Validation a b) deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
  (AccumulateBoth (Success a1)) <> (AccumulateBoth (Success a2)) =
    AccumulateBoth . Success $ a1 <> a2
  (AccumulateBoth (Failure a1)) <> (AccumulateBoth (Failure a2)) =
    AccumulateBoth . Failure $ a1 <> a2
  f@(AccumulateBoth (Failure a)) <> _                              = f
  _                              <> f@(AccumulateBoth (Failure a)) = f

-- Monoid Exercises

-- 1.
instance Monoid Trivial where
  mempty = Trivial

-- 2.
instance (Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty

-- 3.
instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty

-- 4.
instance Monoid BoolConj where
  mempty = BoolConj True

-- 5.
instance Monoid BoolDisj where
  mempty = BoolDisj False

-- 6.
instance (Monoid b) => Monoid (Combine a b) where
  mempty = Combine mempty

-- 7.
instance Monoid (Comp a) where
  mempty = Comp id

-- 8.
newtype Mem s a = Mem { runMem :: s -> (a,s) }

instance Semigroup a => Semigroup (Mem s a) where
  (Mem f1) <> (Mem f2) = Mem $ \s ->
    let (a1, s1) = f1 s
        (a2, s2) = f2 s1
    in  (a1 <> a2, s2)

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem (mempty, )
