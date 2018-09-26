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

ejer2::[Int]->Bool
ejer2 (x1: x2:x3:x4:xs) = (x1+x2+x3+x4)<10
ejer2 _ = False

ejer3::String->String
ejer3 [] = "No se ha introducido una frase" --Failsafe in case of empty array
ejer3 s = "La primera letra de la frase '" ++s++ "' es '"++[head s]++ "' y la ultima letra es '"++[last s] 
--We make those characters a list so we can concat a string with another string (remember String = [Char])

ejer4::String->Char->Int
ejer4 str c = length [x |x<-str,x==c]
