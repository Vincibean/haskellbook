{-# LANGUAGE BangPatterns #-}

module Ch27.MakeItBottom where

-- Make the expression bottom
x = undefined
y = "blah"
main = do
    print (snd (x, x `seq` y))

main' = do
    let !b = x
    print (snd (b, y))