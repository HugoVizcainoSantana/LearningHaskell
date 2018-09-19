module Week1 where

descomponer:: Float -> (Integer,Integer)
descomponer x = (truncate x, truncate(x*100) - truncate x *100)

precioAbono::Int->Int
precioAbono edad 
	| edad<=10 || edad>=65 = 0 
	| edad>10 && edad<=25 = 20 
	| otherwise = 100