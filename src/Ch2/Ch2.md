# Chapter 2
## Comprehension Check

1. Given the following lines of code as they might appear in a source file, how would you change them to use them directly in the REPL?
  - ```half x = x / 2```
  ```let half x = x / 2```
  - ```square x = x * x```
  ```let square x = x * x```

2. Write one function that can accept one argument and work for all the following expressions. Be sure to name the function.
  ```3.14 * (5 * 5)```
  ```3.14 * (10 * 10)```
  ```3.14 * (2 * 2)```
  ```3.14 * (4 * 4)```
  ```
  pify x = 3.14 * x
  pify' x y = 3.14 * (x * y)
  pify'' x = 3.14 * (x * x)
  ```
3. There is a value in Prelude called `pi`. Rewrite your function to use `pi` instead of `3.14`
  ```
  pify x = pi * x
  pify' x y = pi * (x * y)
  pify'' x = pi * (x * x)
  ```

## Parentheses and Association
Below are some pairs of functions that are alike except for parenthesization. Read them carefully and decide if the parentheses change the results of the function. Check your work in GHCi.

1. 
  a) 8 + 7 * 9
  b) (8 + 7) * 9
Parentheses change the results of the function. `*` has higher precendence than `+`, so adding parentheses makes a difference.
In fact, the result for `a` is 71, while the result for `b` is 135.

1. 
  a) ```perimeter x y = (x * 2) + (y * 2)```
  b) ```perimeter x y = x * 2 + y * 2```
Parentheses don't change the results of the function. `*` has higher precendence than `+`, so adding or removing the parentheses doesn't make any difference.
In fact, the result is always the same.

3. 
  a) ```f x = x / 2 + 9```
  b) ```f x = x / (2 + 9)```
Parentheses change the results of the function. `/` has higher precendence than `+`, so adding parentheses makes a difference.
In fact, the results of these function given the same x are different.

## Heal the Sick
The following code samples are broken and won’t compile. The first two are as you might enter into the REPL; the third is from a sourcefile. Find the mistakes and fix them so that they will.
1. ```let area x = 3. 14 * (x * x)```
The space between `3.` and `14` breaks the code. Removing the space does the trick: ```let area x = 3.14 * (x * x)```
2. ```let double x = b * 2```
This function takes a parameter (`x`) and never uses; instead, it uses a (undefined) variable called `b`. In order to fix this code we ca  either rename `b` into `x` or rename `x` into `b`: ```let double x = x * 2```
3. 
```
x = 7
 y = 10
f = x + y
```
The space before the `y` causes a parsing error; since there's no relaionship between the first declaration (`x = 7`) and the second declaration (`y = 10`) we can safely remove the space:
```
x = 7
y = 10
f = x + y
```

## A Head Code
Now for some exercises. First, determine in your head what the following expressions will return, then validate in the REPL:
1. ```let x = 5 in x```
We substitute `x` with `5` and simply get `5`.
2. ```let x = 5 in x * x```
We substitute `x` with `5` and we get `5 * 5` which can be reduced to `25`.
3. ```let x = 5; y = 6 in x * y```
We substitute `x` with `5` and `y` with `6` and we get `5 * 6` which can be reduced to `30`.
4. ```let x = 3; y = 1000 in x + 3```
We substitute `x` with `3` and we get `3 + 3` which can be reduced to `6`. `y` is discarded.

## Parenthesization
Given what we know about the precedence of (*), (+), and (^), how can we parenthesize the following expressions more explicitly without changing their results? Put together an answer you think is correct, then test in the GHCi REPL.
1. ```2 + 2 * 3 - 1```
```2 + (2 * 3) - 1```
2. ```(^) 10 $ 1 + 1```
```10 ^ (1 + 1)```
3. ```2 ^ 2 * 4 ^ 5 + 1```
```((2 ^ 2) * (4 ^ 5)) + 1```

## Equivalent expressions
Which of the following pairs of expressions will return the same result when evaluated? Try to reason them out in your head by reading the code and then enter them into the REPL to check your work:
1. ```1 + 1```; ```2```
`1 + 1` can be reduced to `2`, which is exactly the value of the second expression.
2. ```10 ^ 2```; ```10 + 9 * 10```
`10 + 9 * 10` can be turned into `10 + (9 * 10)`, which in turn can be turned into `(1 * 10) + (9 * 10)`, which can be simplified into `10 * 10` and then into `10 ^ 2`
3. ```400 - 37```; ```(-) 37 400```
While `-` can be used in prefix notation, the expression equivalent to the first case would be `(-) 400 37`; since `-` isn't commutative, the order matters and the result is different
4. ```100 `div` 3```; ```100 / 3```
`/` is a fractional division, while `div` is an integral division (round down), so the result will be different with respect to its precision.
5. ```2 * 5 + 18```; ```2 * (5 + 18)```
Since `*` has higher precedence than `+`, the two expressions aren't equivalent; in particular, the usage of parentheses in the second expression changes the order of application of the operators, leading to a different result.

## More fun with functions
```
z = 7
y = z + 8
x = y ^ 2
waxOn = x * 5
```
1. 
  - ```10 + waxOn```
  `waxOn` is evaluated to `1125`; the above expression simply adds it to `10` and produces `1135`
  - ```(+10) waxOn```
  Due to Haskell's _sectioning_, `(+10)` is a function that takes a number and returns that number plus 10; so, again, we will get back `1135`
  - ```(-) 15 waxOn```
  Applies the `-` (subtraction) function to `15` and `waxOn` (in this order), thus resulting in `15 - waxOn`, that is `15 - 1125`, that is `-1110`
  - ```(-) waxOn 15```
  Applies the `-` (subtraction) function to `waxOn` and `15` (in this order), thus resulting in `waxOn - 15`, that is `1125 - 15`, that is `1110`
2. Entering this function doesn't result in any weird behavious; due to scopung, the `x` defined in the body of the function isn't the same `x` previously defined in the REPL
3. It will follow lazy evaluation, which can best be represented using the substitution principle as:
  ```
  triple waxOn
  triple (x * 5)
  triple ((y ^ 2) * 5)
  triple ((y ^ 2) * 5)
  triple (((z + 8) ^ 2) * 5)
  triple (((7 + 8) ^ 2) * 5)
  (\x -> x * 3) (((7 + 8) ^ 2) * 5)
  (((7 + 8) ^ 2) * 5) * 3
  ((15 ^ 2) * 5) * 3
  (225 * 5) * 3
  1125 * 3
  3375
  ```
7. What is the result of `waxOff 10` or `waxOff (-50)` ?
  - `waxOff 10` = `30`
  - `waxOff (-50)` = `-150`