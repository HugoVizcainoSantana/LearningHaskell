module Week1 where

import Data.Char;

--Type Declaration and Operations
descomponer:: Float -> (Integer,Integer)
descomponer x = (truncate x, truncate(x*100) - truncate x *100)

--Guards
precioAbono::Int->Int
precioAbono edad 
 | edad<=10 || edad>=65 = 0 
 | edad>10 && edad<=25 = 20 
 | otherwise = 100

--Lists Example
listaHasta::Int->[Int]
listaHasta x= [1..x]

listaParesHasta::Int->[Int]
listaParesHasta x = [0,2..x]

listaInfinita = [1,4..]

invertirLista x= reverse x

--Lists By Comprehension
listaCuadradosHasta num = [x*x|x<-[1..num]]

listaTuplas =  [(x,y)| x <- [1..3], y<- [1..2]]

--Exercise With Lists
factorsOfNumber::Integer->[Integer]
factorsOfNumber num = [x|x<-[1..num],num `mod` x == 0]

countUppercase::String->Int
countUppercase arr = length [x|x<-arr,isUpper x]

invertCaseListComprehension::String->String
invertCaseListComprehension s = [if isUpper letra then toLower letra else toUpper letra | letra<-s] 
 
