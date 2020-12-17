module Ch28.Ch28 where

import           Criterion.Main
import qualified Data.Map.Strict               as M
import           Data.Sequence (Seq(..), ViewR( (:>) ), (<|), viewr, empty)
import qualified Data.Set                      as S
import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as UV

-- Benchmark practice

bumpIt (i, v) = (i + 1, v + 1)

m :: M.Map Int Int
m = M.fromList $ take 10000 stream where stream = iterate bumpIt (0, 0)

s :: S.Set Int
s = S.fromList $ take 10000 stream where stream = iterate (+ 1) 0

membersMap :: Int -> Bool
membersMap i = M.member i m

membersSet :: Int -> Bool
membersSet i = S.member i s

main :: IO ()
main = defaultMain
  [ bench "member check map" $ whnf (`M.member` m) 9999
  , bench "member check set" $ whnf (`S.member` s) 9999
  , bench "member check union map" $ whnf (`M.member` (m `M.union` m)) 9999
  , bench "member check union set" $ whnf (`S.member` (s `S.union` s)) 9999
  ]

-- benchmarking member check map
-- time                 29.51 ns   (29.29 ns .. 29.72 ns)
--                      1.000 R²   (0.999 R² .. 1.000 R²)
-- mean                 29.52 ns   (29.31 ns .. 29.98 ns)
-- std dev              963.9 ps   (511.8 ps .. 1.814 ns)
-- variance introduced by outliers: 53% (severely inflated)

-- benchmarking member check set
-- time                 29.52 ns   (29.39 ns .. 29.64 ns)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 29.43 ns   (29.31 ns .. 29.61 ns)
-- std dev              463.6 ps   (373.9 ps .. 694.4 ps)
-- variance introduced by outliers: 20% (moderately inflated)

-- benchmarking member check union map
-- time                 29.68 ns   (29.29 ns .. 30.12 ns)
--                      0.999 R²   (0.999 R² .. 1.000 R²)
-- mean                 29.45 ns   (29.31 ns .. 29.69 ns)
-- std dev              582.4 ps   (401.1 ps .. 960.4 ps)
-- variance introduced by outliers: 29% (moderately inflated)

-- benchmarking member check union set
-- time                 29.59 ns   (29.34 ns .. 29.89 ns)
--                      0.999 R²   (0.999 R² .. 1.000 R²)
-- mean                 29.57 ns   (29.37 ns .. 30.05 ns)
-- std dev              986.8 ps   (481.6 ps .. 1.847 ns)
-- variance introduced by outliers: 54% (severely inflated)



-- TODO 1135 - 1153
--  Vector
boxedVector :: V.Vector Int
boxedVector = V.fromList [1..99999]

unboxedVector :: UV.Vector Int
unboxedVector = UV.fromList [1..99999]

main''' = defaultMain [
    bench "boxed vector length" $ whnf V.length boxedVector
  , bench "unboxed vector length" $ whnf UV.length unboxedVector
  ]

-- benchmarking boxed vector length
-- time                 4.721 ns   (4.534 ns .. 4.869 ns)
--                      0.995 R²   (0.993 R² .. 0.998 R²)
-- mean                 4.573 ns   (4.517 ns .. 4.664 ns)
-- std dev              236.8 ps   (158.7 ps .. 317.3 ps)
-- variance introduced by outliers: 76% (severely inflated)

-- benchmarking unboxed vector length
-- time                 6.769 ns   (6.656 ns .. 6.906 ns)
--                      0.997 R²   (0.996 R² .. 0.998 R²)
-- mean                 6.791 ns   (6.680 ns .. 6.906 ns)
-- std dev              385.1 ps   (337.1 ps .. 460.1 ps)
-- variance introduced by outliers: 79% (severely inflated)



-- Chapter exercises
-- Difference list

-- A difference list representation of a list xs :: [T] is a function f :: [T] -> [T], which when given 
-- another list ys :: [T], returns the list that f represents, prepended to ys. I.e. f ys = xs ++ ys
newtype DList a = DL { unDL :: [a] -> [a] }

-- 1.
empty' :: DList a
empty' = DL id
{-# INLINE empty' #-}

-- 2.
singleton :: a -> DList a
singleton = DL . (:)
{-# INLINE singleton #-}

-- 3. 
toList :: DList a -> [a]
toList dlist = unDL dlist []
{-# INLINE toList #-}

-- 4. Prepend a single element to a dlist.
infixr `cons`
cons :: a -> DList a -> DList a
cons x xs = DL ((x :) . unDL xs)
{-# INLINE cons #-}

-- 5. Append a single element to a dlist.
infixl `snoc`
snoc :: DList a -> a -> DList a
snoc xs x = DL (unDL xs . (x :))
{-# INLINE snoc #-}

-- λ>foo x = id . (x:)
-- λ>foo 1 [2, 3, 4]
-- [1,2,3,4]

-- λ>bar x = (x:) . id
-- λ>bar 1 [2, 3, 4]
-- [1,2,3,4]

-- λ>baz x xs = xs ++ [x]
-- λ>baz 1 [2, 3, 4]
-- [2,3,4,1]

-- 6. Append dlists.
append :: DList a -> DList a -> DList a
append (DL f) (DL f') = DL $ f . f'
{-# INLINE append #-}


schlemiel :: Int -> [Int]
schlemiel i = go i []
 where
  go 0 xs = xs
  go n xs = go (n - 1) ([n] ++ xs)

constructDlist :: Int -> [Int]
constructDlist i = toList $ go i empty'
 where
  go 0 xs = xs
  go n xs = go (n - 1) (singleton n `append` xs)

main' :: IO ()
main' = defaultMain
  [ bench "concat list" $ whnf schlemiel 123456
  , bench "concat dlist" $ whnf constructDlist 123456
  ]

-- benchmarking concat list
-- time                 429.7 μs   (424.6 μs .. 435.8 μs)
--                      0.999 R²   (0.998 R² .. 1.000 R²)
-- mean                 425.1 μs   (421.8 μs .. 430.2 μs)
-- std dev              13.53 μs   (9.359 μs .. 20.30 μs)
-- variance introduced by outliers: 25% (moderately inflated)

-- benchmarking concat dlist
-- time                 257.4 μs   (255.1 μs .. 260.0 μs)
--                      0.999 R²   (0.999 R² .. 1.000 R²)
-- mean                 255.4 μs   (253.4 μs .. 257.3 μs)
-- std dev              6.440 μs   (5.047 μs .. 8.196 μs)
-- variance introduced by outliers: 18% (moderately inflated)


-- A simple queue
data Queue a = Queue { enqueue :: [a] , dequeue :: [a] } deriving (Eq, Show)

-- adds an item
push :: a -> Queue a -> Queue a
push x (Queue ins outs) = Queue (x:ins) outs

push' :: a -> Queue a -> Queue a
push' x (Queue [] []) = Queue [] [x]
push' x (Queue ins outs) = Queue (x:ins) outs

-- Clever solution from Changwoo
pop :: Queue a -> Maybe (a, Queue a)
pop (Queue [] []) = Nothing
pop (Queue e []) = pop (Queue [] (reverse e))
pop (Queue e (x:xs)) = Just (x, Queue e xs)

pop' :: Queue a -> Maybe (a, Queue a)
pop' (Queue [] []) = Nothing
pop' (Queue ins []) = Just (o, Queue [] outs) where (o:outs) = reverse ins
pop' (Queue ins (o:outs)) = Just (o, Queue ins outs)

listQueueBench :: Int -> [Int]
listQueueBench n = foldr go [] [1..2*n]
    where go x acc
            | even x = x : acc
            | otherwise = init acc

queueBench :: Int -> Queue Int
queueBench n = foldr go initQueue [1..2*n]
    where initQueue = Queue [] []
          go x acc
            | even x = push x acc
            | otherwise = let Just (_, acc') = pop acc in acc'

sequenceBench :: Int -> Seq Int
sequenceBench n = foldr go mt [1..2*n]
    where mt = empty :: Seq Int
          go x acc
            | even x = x <| acc -- (<|) : Add an element to the left end of a sequence
            | otherwise = let acc' :> _ = viewr acc in acc'
            -- (:>): the sequence minus the rightmost element
            -- viewr : analyse the right end of a sequence.

main'' = defaultMain
    [ bench "push and pop with a queue" $ whnf queueBench 123456
    , bench "push and pop with a sequence" $ whnf sequenceBench 123456
    ]

-- benchmarking push and pop with a list
-- time                 10.78 ms   (8.276 ms .. 13.03 ms)
--                      0.708 R²   (0.491 R² .. 0.856 R²)
-- mean                 10.57 ms   (9.011 ms .. 12.80 ms)
-- std dev              4.636 ms   (3.341 ms .. 7.095 ms)
-- variance introduced by outliers: 96% (severely inflated)

-- benchmarking push and pop with a queue
-- time                 5.325 ms   (4.371 ms .. 6.457 ms)
--                      0.716 R²   (0.575 R² .. 0.854 R²)
-- mean                 5.134 ms   (4.504 ms .. 6.111 ms)
-- std dev              2.228 ms   (1.377 ms .. 3.703 ms)
-- variance introduced by outliers: 97% (severely inflated)

-- benchmarking push and pop with a sequence
-- time                 4.729 ms   (3.821 ms .. 5.783 ms)
--                      0.711 R²   (0.565 R² .. 0.841 R²)
-- mean                 4.618 ms   (4.062 ms .. 5.528 ms)
-- std dev              2.099 ms   (1.326 ms .. 3.272 ms)
-- variance introduced by outliers: 97% (severely inflated)
