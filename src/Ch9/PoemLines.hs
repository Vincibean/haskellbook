module Ch9.PoemLines where
    
import Ch9.Ch9 (mySplit)

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen 

-- putStrLn sentences 
-- should print
-- Tyger Tyger, burning bright
-- In the forests of the night
-- What immortal hand or eye
-- Could frame thy fearful symmetry?

-- Implement this
myLines :: String -> [String] 
myLines "" = []
myLines xs = word : myLines rest
  where (word, rest) = span (/= '\n') $ dropWhile (== '\n') xs

myLines' :: String -> [String] 
myLines' = mySplit '\n'

-- What we want 'myLines sentences' to equal
shouldEqual = 
    [ "Tyger Tyger, burning bright"
    , "In the forests of the night"
    , "What immortal hand or eye"
    , "Could frame thy fearful symmetry?"
    ]

-- The main function here is a small test
-- to ensure you've written your function
-- correctly.
main :: IO ()
main = 
    print $ "Are they equal? " 
            ++ show (myLines' sentences == shouldEqual)