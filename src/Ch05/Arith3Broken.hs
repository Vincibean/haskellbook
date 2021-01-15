module Ch05.Arith3Broken where

main :: IO ()
main = do
    print $ 1 + 2
    putStrLn $ show 10
    print $ show (negate (-1))
    print ((+) 0 blah)
      where blah = negate 1
