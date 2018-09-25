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
 
 --Exercises week 2 

 --{
 --   Implement a function receiving a 3-tuple whose elemets are another 2-tuple with elements String and Int
 --   It should output a 3-tuple containing the first element of each nested 2-tuple
 --   Inputs :      ((String,Int),(String,Int),(String,Int))
 --   Outputs:      (String,String,String)
 --}
 
ejer1::((String,Int),(String,Int),(String,Int))->(String,String,String)
ejer1 ((s1,_),(s2,_),(s3,_)) = (s1,s2,s3)