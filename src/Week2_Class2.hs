--
-- Back to recursive!
--

module Week2_Class2 where

import Data.Char;

factorial::Int->Int
factorial 0 = 1
factorial n = n* factorial (n-1)

fibonacci::Int->Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci(n-1)+fibonacci(n-2)


factorialTailRecursive::Int->Int
factorialTailRecursive'::(Int,Int)->Int

factorialTailRecursive n = factorialTailRecursive'(n,1)
factorialTailRecursive' (1,r) = r
factorialTailRecursive' (n,r) = factorialTailRecursive'(n-1,n*r)


-- Exercises
sumOfList::[Int]->Int
sumOfList [] = 0
-- Old way: sumOfList list = head list + sumOfList(tail list)
sumOfList (n:ns) = n+sumOfList ns

sumOfListTailRecursive::[Int]->Int
sumOfListTailRecursive'::([Int],Int)->Int

sumOfListTailRecursive list = sumOfListTailRecursive'(list,0)
sumOfListTailRecursive' ([],sum) = sum
sumOfListTailRecursive' ((n:ns),sum)= sumOfListTailRecursive'(ns,sum+n)

invertList::[Char]->[Char]
invertList [] = []
invertList x = last x:invertList(init x)

numInList::[Int]->Int->Bool
numInList [] _ = False
numInList (y:ys) x = (x==y) || (numInList ys x)

replaceNum::Int->Int->[Int]->[Int]
replaceNum _ _ [] = []
replaceNum x y (n:ns) = if (x==n) then (y:replaceNum x y ns) else (n:replaceNum x y ns)

--{
-- In: [1,2,3] [4,5,6,7,8,9,10,11]
-- Out: [(1,4,5),(2,6,7),(3,8,9)]
--}
ternas::[Int]->[Int]->[(Int,Int,Int)]
ternas [] _ = []
ternas _ [] = []
ternas (x:xs) (y1:y2:ys) = (x,y1,y2):ternas xs ys
--Ugly way: ternas (x:xs) (y:ys) = []
ternas _ [y] = []


--{
-- In: [1,2,3,1,5,4,2,3]
-- Out: [(1,0),(2,1),(3,2),(5,4),(4,5)]
--}
firstAppearence::[Int]->[(Int,Int)]
firstAppearence x = firstAppearence' x [] 0

firstAppearence'::[Int]->[Int]->Int->[(Int,Int)]
firstAppearence' [] _ _ = []
firstAppearence' (x:xs) vistos iter = 
    if not(numInList vistos x) then
        (x,iter):(firstAppearence' xs (x:vistos) (iter+1))
    else 
        firstAppearence' xs vistos (iter+1)
        
        
--{
-- Count Sequences of Consecutive Zeros
-- In: [0,1,2,0,0,4,5,6,0,0,0,7,6,0,5,6]
-- Out: 4
--}
consecutiveZeros::[Int]->Int
consecutiveZeros [] = 0
consecutiveZeros (0:x:xs)
consecutiveZeros (x1:0:xs) = 
    if (x==0) then consecutiveZeros (x2:xs) 
    else 1+ consecutiveZeros (x2:xs)







