{- |
Module                  : Lecture1
Copyright               : (c) 2021-2022 Haskell Beginners 2022 Course
SPDX-License-Identifier : MPL-2.0
Maintainer              : Haskell Beginners 2022 Course <haskell.beginners2022@gmail.com>
Stability               : Stable
Portability             : Portable

Exercises for the Lecture 1 of the Haskell Beginners course.

To complete exercises, you need to complete implementation and add
missing top-level type signatures. You can implement any additional
helper functions. But you can't change the names of the given
functions.

Comments before each function contain explanations and example of
arguments and expected returned values.

It's absolutely okay if you feel that your implementations are not
perfect. You can return to these exercises after future lectures and
improve your solutions if you see any possible improvements.
-}

module Lecture1
    ( makeSnippet
    , sumOfSquares
    , lastDigit
    , minmax
    , subString
    , strSum
    , lowerAndGreater
    ) where

{- | Specify the type signature of the following function. Think about
its behaviour, possible types for the function arguments and write the
type signature explicitly.
-}
makeSnippet :: Int -> [Char] -> [Char]
makeSnippet limit text = take limit ("Description: " ++ text) ++ "..."

{- | Implement a function that takes two numbers and finds sum of
their squares.

>>> sumOfSquares 3 4
25

>>> sumOfSquares (-2) 7
53

Explanation: @sumOfSquares 3 4@ should be equal to @9 + 16@ and this
is 25.
-}
-- DON'T FORGET TO SPECIFY THE TYPE IN HERE
square :: Int -> Int
square x = x * x

sumOfSquares :: Int -> Int -> Int
sumOfSquares x y = square x + square y

{- | Implement a function that returns the last digit of a given number.

>>> lastDigit 42
2
>>> lastDigit (-17)
7

🕯 HINT: use the @mod@ function

-}
-- DON'T FORGET TO SPECIFY THE TYPE IN HERE
lastDigit :: Int -> Int
lastDigit n = if n < 0 
    then (mod n (-10)) * (-1)
    else mod n 10 

{- | Write a function that takes three numbers and returns the
difference between the biggest number and the smallest one.

>>> minmax 7 1 4
6

Explanation: @minmax 7 1 4@ returns 6 because 7 is the biggest number
and 1 is the smallest, and 7 - 1 = 6.

Try to use local variables (either let-in or where) to implement this
function.
-}

-- max :: Int -> Int -> Int
-- max x y = if x < y then y else x

-- min :: Int -> Int -> Int
-- min x y = if x > y then y else x

minmax :: Int -> Int -> Int -> Int
minmax x y z = 
    let mx = max (max x y) z
        mn = min (min x y) z
    in mx - mn

{- | Implement a function that takes a string, start and end positions
and returns a substring of a given string from the start position to
the end (including).

>>> subString 3 7 "Hello, world!"
"lo, w"

>>> subString 10 5 "Some very long String"
""

This function can accept negative start and end position. Negative
start position can be considered as zero (e.g. substring from the
first character) and negative end position should result in an empty
string.
-}
subString :: Int -> Int -> [Char] -> [Char]
subString start end str = 
    let startIndex = if start < 0 then 0 else start
    in if end < 0 then "" else (take ((end - startIndex) + 1) (drop startIndex str))

{- | Write a function that takes a String — space separated numbers,
and finds a sum of the numbers inside this string.

>>> strSum "100    -42  15"
73

The string contains only spaces and/or numbers.
-}
strSum :: [Char] -> Int
strSum str = sumList 0 (map (\x -> read (x) :: Int) (words str))
    where 
        sumList result list = 
            if null list
                then result
                else sumList (result + head list) (tail list)

{- | Write a function that takes a number and a list of numbers and
returns a string, saying how many elements of the list are strictly
greated than the given number and strictly lower.

>>> lowerAndGreater 3 [1 .. 9]
"3 is greater than 2 elements and lower than 6 elements"

Explanation: the list [1 .. 9] contains 9 elements: [1, 2, 3, 4, 5, 6, 7, 8, 9]
The given number 3 is greater than 2 elements (1 and 2)
and lower than 6 elements (4, 5, 6, 7, 8 and 9).

🕯 HINT: Use recursion to implement this function.
-}

-- Simpler implementation with let and straiforward functions:
-- lowerAndGreater :: Int -> [Int] -> [Char]
-- lowerAndGreater n list = 
--     let calculateMin x acc l = if null l then acc else calculateMin x (if (head l) < x then (acc + 1) else acc) (tail l)
--         calculateMax x acc l = if null l then acc else calculateMax x (if (head l) > x then (acc + 1) else acc) (tail l)
--     in show n ++ " is greater than " ++ show (calculateMin n 0 list) ++ " elements and lower than " ++ show (calculateMax n 0 list) ++ " elements"

-- Implementation with where clause and operator passing as an argument:
-- lowerAndGreater :: Int -> [Int] -> [Char]
-- lowerAndGreater n list = show n ++ " is greater than " ++ show (calculateMin n 0 list) ++ " elements and lower than " ++ show (calculateMax n 0 list) ++ " elements"
--     where 
--         calculate :: Int -> Int -> [Int] -> (Int -> Int -> Bool) -> Int
--         calculate x acc l op = if null l then acc else (calculate x (if (op (head l) x) then (acc + 1) else acc) (tail l) op)
--         calculateMin :: Int -> Int -> [Int] -> Int
--         calculateMin x acc l = calculate x acc l (>)
--         calculateMax :: Int -> Int -> [Int] -> Int
--         calculateMax x acc l = calculate x acc l (<)

-- Implementation with single list traversal
lowerAndGreater :: Int -> [Int] -> [Char]
lowerAndGreater n list = show n ++ " is greater than " ++ show mn ++ " elements and lower than " ++ show mx ++ " elements"
    where 
        getMinMax :: Int -> Int -> Int -> [Int] -> (Int, Int)
        getMinMax x accMin accMax l 
            | null l = (accMin, accMax) 
            | (head l) > x = getMinMax x accMin (accMax + 1) (tail l)
            | (head l) < x = getMinMax x (accMin + 1) accMax (tail l)
            | otherwise = getMinMax x accMin accMax (tail l)
        (mn, mx) = getMinMax n 0 0 list 

