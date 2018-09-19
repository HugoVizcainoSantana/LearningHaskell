module Week1 where

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