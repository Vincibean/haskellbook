module Ch14.Ch14 where

-- Intermission: Short Exercise

sumRec :: Integral a => a -> a -> a
sumRec x y
  | x == 0 || y == 0 = 0
  | y == 1 = x
  | x == 1 = y
  | x < 0 = sumRec (- x) y
  | y < 0 = sumRec x (- y)
  | otherwise = x + sumRec x (y - 1)
