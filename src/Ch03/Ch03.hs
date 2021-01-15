module Ch03.Ch03 where

-- Building Functions

-- 2

-- Given "Curry is awesome" return "Curry is awesome!"
a :: String -> String
a x = x ++ "!"

-- Given "Curry is awesome!" return "y"
b :: String -> String
b x = x !! 4 : ""

-- Given "Curry is awesome!" return "awesome!"
c :: String -> String
c x = drop 9 x

-- 3

-- If you apply your function to this value: "Curry is awesome" Your function should return 'r'
thirdLetter :: String -> Char
thirdLetter x = x !! 2

thirdLetter' :: String -> Char
thirdLetter' = (!! 2)

-- 4

letterIndex :: Int -> Char
letterIndex x = "Curry is awesome!" !! x

letterIndex' :: Int -> Char
letterIndex' = ("Curry is awesome!" !!)

-- 5

-- Given "Curry is awesome" return "awesome is Curry"
rvrs :: String -> String
rvrs x = awesome ++ " " ++ is ++ " " ++ curry
  where awesome = take 7 $ drop 9 x
        is = take 2 $ drop 6 x
        curry = take 5 x