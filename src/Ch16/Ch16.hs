{-# LANGUAGE FlexibleInstances #-}

module Ch16.Ch16 where

-- Be Kind
-- Given a type signature, determine the kinds of each type variable:
-- 1. What’s the kind of a?
--    a -> a
--    a :: * 
-- 2. What are the kinds of b and T ? (The T is capitalized on purpose!)
--    a -> b a -> T (b a)
--    b :: * -> *
--    T :: * -> *
-- 3. What’s the kind of c?
--    c a b -> c b a
--    c :: * -> * -> *

-- Wait, how does that even typecheck?

-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- fmap :: Functor f => (m -> n) -> f m -> f n
-- fmap :: Functor g => (x -> y) -> g x -> g y
-- (fmap . fmap) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)


-- fmap :: Functor f => (m -> n) -> (f m -> f n)
--                         b     ->      c
-- fmap :: Functor g => (x -> y) -> (g x -> g y)
--                         a     ->      b

-- fmap :: Functor f => (g x -> g y) -> (f g x -> f g y)
--                         b         ->      c
-- fmap :: Functor g => (x -> y) -> (g x -> g y)
--                         a     ->      b

-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- hence
-- a -> c => (x -> y) -> (f g x -> f g y)
-- or, with functors
-- (Functor f, Functor g) => (x -> y) -> (f g x -> f g y)
-- which can be rewritten as:
-- (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)


-- Heavy Lifting
-- 1.
a = fmap (+ 1) $ read "[1]" :: [Int]
-- 2.
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])
-- 3. 
c = fmap (* 2) (\x -> x - 2)
-- 4.
d = fmap ((return '1' ++) . show) (\x -> [x, 1 .. 3])
-- 5.
e :: IO Integer
e =
  let ioi     = readIO "1" :: IO Integer
      changed = fmap (read . ("123" ++) . show) ioi
  in  fmap (* 3) changed


-- Instances of Func
-- 1. 
newtype Identity a = Identity a deriving (Show, Eq)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

-- 2. 
data Pair a = Pair a a deriving (Show, Eq)

instance Functor Pair where
  fmap f (Pair l r) = Pair (f l) (f r)

-- 3. 
data Two a b = Two a b deriving (Show, Eq)

instance Functor (Two a) where
  fmap f (Two a b) = Two a $ f b

-- 4. 
data Three a b c = Three a b c deriving (Show, Eq)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b $ f c

-- 5. 
data Three' a b = Three' a b b deriving (Show, Eq)

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

-- 6. 
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c $ f d

-- 7. 
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a a' a'' b) = Four' a a' a'' $ f b

-- 8. 
-- Can you implement one for this type? Why? Why not?

data Trivial = Trivial deriving (Eq, Show)

--    We cannot implement a Functor for the Trivial type because the kind of Trivial isn't high enough
--    (Functor requires a type whose kind is * -> * while Trivial has kind *)
--    What we can do, though, is trick the compiler with a phantom type

newtype Trivial' a = Trivial' { getTrivial :: Trivial } deriving (Eq, Show)

instance Functor Trivial' where
  fmap _ _ = Trivial' Trivial

-- Possibly

data Possibly a = LolNope | Yeppers a deriving (Eq, Show)

instance Functor Possibly where
  fmap _ LolNope     = LolNope
  fmap f (Yeppers a) = Yeppers $ f a

-- Short Exercise

-- 1.
data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First  a) = First a
  fmap f (Second b) = Second $ f b

-- 2. Why is a Functor instance that applies the function only to First, Either’s Left, impossible? We covered this earlier.
--    A Functor applies to any data structure with kind * -> * ; Sum has kind * -> * -> * ; in order to bring it down to the right kind,
--    we need to apply a type to it; we apply a, the type argument of First; hence, First a becomes part of the Functor's "structure" and
--    we cannot touch a anymore.

-- Chapter exercises
-- Determine if a valid Functor can be written for the datatype provided.

-- 1. data Bool = False | True
--    No: the kind isn't right (should be * -> * ; it is *)

-- 2. data BoolAndSomethingElse a = False' a | True' a
--    Yes: the kind is right.

-- 3. data BoolAndMaybeSomethingElse a = Falsish | Truish a
--    Yes: the kind is right.

-- 4. newtype Mu f = InF { outF :: f (Mu f) }
--    No: the kind isn't right (should be * -> * ; it is  (* -> *) -> * )

newtype Mu f = InF { outF :: f (Mu f) }

-- 5. data D = D (Array Word Word) Int Int
--    No: the kind isn't right (should be * -> * ; it is *)

-- Rearrange the arguments to the type constructor of the datatype so the Functor instance works.
-- 1. 
data Sum' b a = First' a | Second' b

instance Functor (Sum' e) where
  fmap f (First'  a) = First' (f a)
  fmap f (Second' b) = Second' b

-- 2. 
data Company a b c = DeepBlue a b | Something c

instance Functor (Company e e') where
  fmap f (Something b ) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

-- 3. 
data More b a = L a b a | R b a b deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'


-- Write Functor instances for the following datatypes.
-- 1. 
data Quant a b = Finance | Desk a | Bloor b deriving (Show, Eq)

instance Functor (Quant a) where
  fmap _ Finance   = Finance
  fmap _ (Desk  a) = Desk a
  fmap f (Bloor b) = Bloor $ f b

-- 2. No, it’s not interesting by itself.
data K a b = K a deriving (Show, Eq)

instance Functor (K a) where
  fmap _ (K a) = K a

-- 3.
newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

newtype K' a b = K' a deriving (Eq, Show)

instance Functor (Flip K' a) where
  fmap f (Flip (K' a)) = Flip $ K' $ f a

-- 4.
data EvilGoateeConst a b = GoatyConst b deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst $ f b

-- 5.
data LiftItOut f a = LiftItOut (f a) deriving (Eq, Show)

instance (Functor f) => Functor (LiftItOut f) where
  fmap f (LiftItOut b) = LiftItOut $ f <$> b

-- 6. 
data Parappa f g a = DaWrappa (f a) (g a) deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa x y) = DaWrappa (f <$> x) (f <$> y)

-- 7.
data IgnoreOne f g a b = IgnoringSomething (f a) (g b) deriving (Eq, Show)

instance (Functor g) => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething a b) = IgnoringSomething a $ f <$> b

-- 8. 
data Notorious g o a t = Notorious (g o) (g a) (g t) deriving (Eq, Show)

instance (Functor g) => Functor (Notorious g o a) where
  fmap f (Notorious g g' g'') = Notorious g g' $ f <$> g''

-- 9.
data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil           = Nil
  fmap f (Cons a list) = Cons (f a) (f <$> list)

-- 10.
data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a) deriving (Eq, Show)

instance Functor GoatLord where
  fmap _ NoGoat      = NoGoat
  fmap f (OneGoat a) = OneGoat $ f a
  fmap f (MoreGoats gl gl' gl'') =
    MoreGoats (f <$> gl) (f <$> gl') (f <$> gl'')

-- 11. 
-- You’ll use an extra functor for this one, although your solution
-- might do it monomorphically without using fmap. Keep in
-- mind that you will probably not be able to validate this one in
-- the usual manner. Do your best to make it work.
data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt        = Halt
  fmap f (Print s a) = Print s $ f a
  fmap f (Read fun ) = Read $ f <$> fun
