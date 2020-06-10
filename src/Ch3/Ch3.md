# Chapter 3
## Scope
1. These lines of code are from a REPL session. Is `y` in scope for `z`?
  ```
  Prelude> let x = 5
  Prelude> let y = 7
  Prelude> let z = x * y
  ```
Yes. The REPL evaluates everything sequentially. Since `y` is defined before `z`, everything works as expected.
2. These lines of code are from a REPL session. Is ℎ in scope for function 𝑔? Go with your gut here.
  ```
  Prelude> let f = 3
  Prelude> let g = 6 * f + h
  ```
No. `h` isn't defined anywhere, so `g` can't be evaluated and a compile time error is thrown.
3. This code sample is from a source file. Is everything we need to execute `area` in scope?
  ```
  area d = pi * (r * r)
  r = d / 2
  ```
No. `d` isn't defined anywhere, so `area` can't be defined and a compile time error is thrown, and `d`. The `d` defined in `area` and the `d` defined in `r` belong to different scopes.
4. This code is also from a source file. Now are `r` and `d` in scope for `area`?
  ```
  area d = pi * (r * r)
    where r = d / 2
  ```
Yes, now `r`, `d` and `area` all belong to the same scope, so the expression compiles.

## Syntax Errors
Read the syntax of the following functions and decide whether it will compile. Test them in your REPL and try to fix the syntax errors where they occur.
1. ```++ [1, 2, 3] [4, 5, 6]```
This won't compile: `++` is an infix function, but here it is used in prefix position, in order to fix, we can either use it in infix position, or use parens to use it in prefix position:
- ```[1, 2, 3] ++ [4, 5, 6]```
- ```(++) [1, 2, 3] [4, 5, 6]```

2. ```'<3' ++ ' Haskell'```
This won't compile: single quotes (`'`) denote a character, not a string; in order to make it compile, we have to use the appropriate quotes:
```"<3" ++ " Haskell"```
3. ```concat ["<3", " Haskell"]```
This compiles.

## Reading syntax
1. For the following lines of code, read the syntax carefully and decide if they are written correctly. 
Test them in your REPL after you’ve decided to check your work. Correct as many as you can.
a) ```concat [[1, 2, 3], [4, 5, 6]]```
It will compile
b) ```++ [1, 2, 3] [4, 5, 6]```
This won't compile: `++` is an infix function, but here it is used in prefix position, in order to fix, we can either use it in infix position, or use parens to use it in prefix position:
c) ```(++) "hello" " world"```
It will compile
d) ```["hello" ++ " world]```
It won't compile: missing double quote; if a double quote was included, then it would compile and give back a list of only one string ("hello world")
e) ```4 !! "hello"```
It won't compile: the arguments are swapped
f) ```(!!) "hello" 4```
It will compile
g) ```take "4 lovely"```
It won't compile: `take` takes (no pun intended) an integer and a list; arguably, `4` shouldn be passed in (as a number) as the first argument.
h) ```take 3 "awesome"```
It will compile

2. 
a) ```concat [[1*6], [2*6], [3*6]]```
```[6,12,18]```
b) ```"rain" ++ drop 2 "elbow"```
```"rainbow"```
c) ```10 * head [1, 2, 3]```
```10```
d) ```(take 3 "Julie") ++ (tail "yes")```
```"Jules"```
e)```
  concat [tail [1, 2, 3],
          tail [4, 5, 6],
          tail [7, 8, 9]]
  ```
  ```[2,3,5,6,8,9]```