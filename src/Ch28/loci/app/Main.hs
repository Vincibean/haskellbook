-- stack build
-- .stack-work/dist/x86_64-linux-tinfo6/Cabal-3.0.1.0/build/loci-exe/loci-exe +RTS -hc -p
-- hp2ps loci-exe.hp

module Main where

import           Criterion.Main

import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as UV

boxedVector :: V.Vector Int
boxedVector = V.fromList [1..99999]

unboxedVector :: UV.Vector Int
unboxedVector = UV.fromList [1..99999]

main :: IO ()
-- main = defaultMain [ bench "boxed vector length" $ whnf V.length boxedVector ]
main = defaultMain [ bench "unboxed vector length" $ whnf UV.length unboxedVector ]