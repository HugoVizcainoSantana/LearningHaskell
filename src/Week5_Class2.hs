module Week5_Class2 where

import Data.Char;

type Comensales = Int
data Mesa = Mesa{posicion::Int,capacidad::Comensales}
instance Show Mesa where
	show (Mesa ident capacidad) = "Mesa " ++ show ident ++ "Capacidad:" ++ show capacidad
instance Eq Mesa where
	(Mesa ident1 asientos1) == (Mesa ident2 asientos2) =( asientos1==asientos2)
data Restaurante = Restaurante{mesasLibres::[Mesa],mesasOcupadas::[Mesa]}

insertarMesaLibre::Mesa->Restaurante->Restaurante
insertarMesaLibre mesa (Restaurante [] ocupadas) = (Restaurante [mesa] ocupadas)
insertarMesaLibre mesa (Restaurante libres ocupadas) = Restaurante (insertarMesaOrdenada mesa libres) ocupadas

insertarMesaOrdenada::Mesa->[Mesa]->[Mesa]
insertarMesaOrdenada mesa [] = [mesa]
insertarMesaOrdenada mesa (primeraLibre:restoLibres)
	| (capacidad mesa)>=(capacidad primeraLibre) = primeraLibre:(insertarMesaOrdenada mesa restoLibres)
	| otherwise = mesa:primeraLibre:restoLibres
	
ocuparMesa :: Restaurante->Comensales->Restaurante
ocuparMesa restaurante comensales = ocuparMesaAux restaurante comensales []

ocuparMesaAux:: Restaurante->Comensales->[Mesa]->Restaurante
ocuparMesaAux (Restaurante (primeraLibre:restoLibres) ocupadas) comensales libresDescartadas 
	| (capacidad primeraLibre) < comensales = (Restaurante libresDescartadas (ocuparMesaAux (Restaurante restoLibres ocupadas) comensales libresDescartadas) ocupadas)
	| otherwise = (Restaurante (libresDescartadas:restoLibres) (primeraLibre:ocupadas))
	
	
	
	
--New exercise with class and data types
class Coleccion col where --Col es el constructor

	esVacia::col a->Bool
	insertar::a->col a->col a
	primero::col a->a
	eliminar::col a->col a
	size::col a->Int

data Pila a = Pila [a] deriving Show
instance Coleccion Pila where
	esVacia (Pila a) = (length a == 0)
	insertar x (Pila a) = Pila (x:a)
	primero (Pila (primero:resto)) = primero
	eliminar (Pila (primero:resto)) = Pila resto
	size (Pila a) = length a
	
data Cola a = Cola [a] deriving Show	
instance Coleccion Cola where
	esVacia (Cola a) = (length a == 0)
	insertar x (Cola a) = Cola (x:a)
	primero (Cola a) = last a
	eliminar (Cola a) = Cola (init a)
	size (Cola a) = length a
	
--Exercise about text processing	
data Acronimo = Acron{acronimo::String} | FormaExt{formaExtendida::[String]}

instance Eq Acronimo where
	(Acron acr)==(FormaExt fExt)=True 

instance Show Acronimo where
	show (Acron acr)= "Acron "++acr
	show (FormaExt fe)= "FormaExt "++show fe       
	
insertarAcronimo::[Acronimo]->Acronimo->[Acronimo]
insertarAcronimo [] acr = [acr]
insertarAcronimo listaAcr acr = acr:listaAcr















	
	
	
