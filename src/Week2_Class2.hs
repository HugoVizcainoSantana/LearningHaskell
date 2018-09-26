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

numInList::Int->[Int]->Bool
numInList _ [] = False
numInList x (y:ys) = (x==y) || (numInList x ys)

replaceNum::Int->Int->[Int]->[Int]
replaceNum _ _ [] = []
replaceNum x y (n:ns) = 
    if (x==n) then (y:replaceNum x y ns) else n:replaceNum x y ns










