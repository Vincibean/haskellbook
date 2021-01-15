module Ch03.Print3 where 
    
printSecond :: IO ()
printSecond = do 
    putStrLn greeting 
    
main :: IO ()
main = do
    putStrLn greeting
    printSecond

greeting :: String
greeting = "Yarrrrr"