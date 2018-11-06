
module Week6_Class1 where

import Data.Char;

escribirTabla::[String]-> IO()
escribirTabla x = escribirTablaAux x 1

escribirTablaAux::[String]->Int-> IO()
escribirTablaAux [] _ = return ()
	
escribirTablaAux (x:xs) num = do
	putStrLn (show(num)++": "++x)
	escribirTablaAux xs (num+1)
	
comparaCadenas::IO()
comparaCadenas = do
	putStrLn ("Introduzca la primera cadena: ")
	cad1 <- getLine
	putStrLn ("Introduzca la segunda cadena: ")
	cad2 <- getLine
	if(cad1 `esIgual` cad2) then
		putStrLn ("Las cadenas son iguales")
	else
		putStrLn ("Las cadenas son diferentes")
		
esIgual::(Eq a)=>[a]->[a]->Bool
esIgual [] [] = True
esIgual [] _ = False
esIgual _ [] = False
esIgual (x:xs) (y:ys) = (x==y) && esIgual xs ys

escribirFichero::IO()
escribirFichero = do
	putStrLn ("Introduzca el nombre del fichero")
	nombre <- getLine
	writeFile nombre "Test"
	
invertirFichero::IO()
invertirFichero = do
	putStrLn ("Introduzca el nombre del fichero")
	nombre <- getLine
	contenido <- readFile nombre
	writeFile ("Invertido-"++nombre) (foldl(\result x-> x:result) [] contenido)
	
leerLineas::IO()
leerLineas = do
	putStr ("Introduce una linea: ")
	linea <- getLine
	if (linea == "") then do
		return ()
	else do 
		putStrLn ("La linea tiene " ++ (show (length linea)) ++ " caracter(es)")	
		leerLineas
		
copiaFichero::String->String->IO()
copiaFichero fichero1 fichero2 = do
    contenidoFichero1 <- readFile fichero1
    writeFile fichero2 contenidoFichero1
    
    
data Operacion = Suma|Resta|Multiplicacion|Division

calculadora::Operacion->Int->Int->Int
calculadora Suma x y = x+y
calculadora Resta x y = x-y
calculadora Multiplicacion x y = x * y
calculadora Division x y = x `div` y

separarPorX::IO([Int],Int,[[Int]])
separarPorX = do
    putStrLn ("Introduzca la lista de datos")
    arr<-getLine
    putStr ("Introduzca el caracter por el que separar la lista")
    sep<-getChar
    let result = separarPorXAux arr sep
    putStrLn ("Resultado:"++show(result))
    return result
    
separarPorXAux::[Int]->Int->[[Int]]
separarPorXAux arr sep = (foldl(\result x-> 
                            if (x==sep) then do
                                []:result
                            else do
                                (insertarEnPrimeraSublista x result)
                            ) [] arr)
                            
insertarEnPrimeraSublista::Int->[[Int]]->[[Int]]  
insertarEnPrimeraSublista x (arrH:arrB) = (x:arrH):arrB                          


	
	