module Ch03.Reverse where

rvrs :: String -> String
rvrs x = awesome ++ " " ++ is ++ " " ++ curry
  where awesome = take 7 $ drop 9 x
        is = take 2 $ drop 6 x
        curry = take 5 x
    
main :: IO ()
main = print $ rvrs "Curry is awesome"