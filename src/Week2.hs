module Week2 where

import Data.Char;

invertCase::String->String
invertCase [] = []
invertCase (x:xs) 
 |isUpper(x) = toLower(x):invertCase(xs)
 |isLower(x) = toUpper(x):invertCase(xs)
 |otherwise = x:invertCase(xs)
 
countUppercaseRecursive::String->Int
countUppercaseRecursive [] = 0
countUppercaseRecursive(x:xs)
 |isUpper(x) = 1+ countUppercaseRecursive(xs)
 |otherwise = countUppercaseRecursive(xs)