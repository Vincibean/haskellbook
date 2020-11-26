module Ch27.Ch27 where

-- Evaluate
-- Expand the following expressions in as much detail as possible. Then, work outside-in to see what the expressions evaluate to:

-- 1. const 1 undefined
--    const 1 (undefined ($dIP_rLzT `cast` <Co:4>))
--    Since const x is a unary function which evaluates to x for all inputs (const :: a -> b -> a ), 
--    this expression will never evaluate to bottom.

-- 2. const undefined 1
--    \ @ a_aLCS -> const (undefined ($dIP1_rLE2 `cast` <Co:4>)) 1
--    Since const x is a unary function which evaluates to x for all inputs (const :: a -> b -> a ), 
--    this expression will always evaluate to bottom.

-- 3. flip const undefined 1
--    flip const (undefined ($dIP2_rLE3 `cast` <Co:4>)) 1
--    Since const x is a unary function which evaluates to x for all inputs (const :: a -> b -> a ), 
--    this expression would always evaluate to bottom; however, `flip` swaps the argument, and hence makes
--    this expression never evaluate to bottom.

-- 4. flip const 1 undefined
--    \ @ c_aLC9 -> flip const 1 (undefined ($dIP3_rLE4 `cast` <Co:4>))
--    Since const x is a unary function which evaluates to x for all inputs (const :: a -> b -> a ), 
--    this expression would never evaluate to bottom; however, `flip` swaps the argument, and hence makes
--    this expression always evaluate to bottom.

-- 5. const undefined undefined
--    \ @ a_aLBM ->
--       const
--         (undefined ($dIP5_rLE6 `cast` <Co:4>))
--         (undefined ($dIP4_rLE5 `cast` <Co:4>))
--    Since const x is a unary function which evaluates to x for all inputs (const :: a -> b -> a ), 
--    this expression will always evaluate to bottom.

-- 6. foldr const 'z' ['a'..'e']
--    foldr
--       $fFoldable[]
--       const
--       (C# 'z'#)
--       (enumFromTo $fEnumChar (C# 'a'#) (C# 'e'#))

--    foldr const 'z' ['a'..'e']
--    foldr const 'e' ['a'..'d']
--    foldr const 'd' ['a'..'c']
--    foldr const 'c' ['a','b']
--    foldr const 'b' ['a']
--    foldr const 'a' []
--    'a'
   
--    (Thank you for this Changwoo!)

--    Since const x is a unary function which evaluates to x for all inputs (const :: a -> b -> a ), 
--    this expression will always evaluate to the "current value" given to foldr. The last element to be evaluated
--    is 'a', so this expression will return 'a'.

-- 7. foldr (flip const) 'z' ['a'..'e']
--    foldr
--       $fFoldable[]
--       (flip const)
--       (C# 'z'#)
--       (enumFromTo $fEnumChar (C# 'a'#) (C# 'e'#))

--    foldr (flip const) 'z' ['a'..'e']
--    foldr (flip const) 'z' ['a'..'d']
--    foldr (flip const) 'z' ['a'..'c']
--    foldr (flip const) 'z' ['a', 'b']
--    foldr (flip const) 'z' ['a']
--    foldr (flip const) 'z' []
--    'z'

--    Since const x is a unary function which evaluates to x for all inputs (const :: a -> b -> a ), 
--    this expression will always evaluate to the "accumulator" given to foldr: 'z'.

-- Chapter exercises
-- What will sprint output?

-- 1. let x = 1
--    :sprint x
--    x = _

-- 2. let x = ['1']
--    :sprint x
--    x = "1"
--    This evaluates because it isn't polymorphic

-- 3. let x = [1]
--    :sprint x
--    x = _
--    This doesn't evaluate because it is polymorphic

-- 4. let x = 1 :: Int
--    :sprint x
--    x = 1

-- 5. let f = \x -> x
--    let x = f 1
--    :sprint f
--    f = _
--    :sprint x
--    x = _

-- 6. let f :: Int -> Int; f = \x -> x
--    let x = f 1
--    :sprint f
--    f = _
--    :sprint x
--    x = _

-- Will printing this expression result in bottom?
-- 1. snd (undefined, 1)
--    No bottom

-- 2. let x = undefined
--    let y = x `seq` 1 in snd (x, y)
--    Bottom

-- 3. length $ [1..5] ++ undefined
--    Bottom

-- 4. length $ [1..5] ++ [undefined]
--    No bottom

-- 5. const 1 undefined
--    No bottom

-- 6. const 1 (undefined `seq` 1)
--    No bottom

-- 7. const undefined 1
--    Bottom