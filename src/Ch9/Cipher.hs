module Ch9.Cipher (caesar, unCaesar) where

import           Data.Char

minOrdUpper :: Int
minOrdUpper = ord 'A'

minOrdLower :: Int
minOrdLower = ord 'a'

alphSize :: Int
alphSize = length ['a' .. 'z']

caesar :: Int -> String -> String
caesar n = map $ shift n

unCaesar :: Int -> String -> String
unCaesar n = map $ shift (-n)

shift n c
  | isLower c = shiftMod minOrdLower c
  | isUpper c = shiftMod minOrdUpper c
  | otherwise = c
  where shiftMod m = chr . (+ m) . (`mod` alphSize) . (+ n) . (subtract m) . ord
