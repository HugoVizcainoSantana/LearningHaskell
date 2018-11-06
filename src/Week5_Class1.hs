module Week5_Class1 where

import Data.Char;

type Comensales = Int
data Mesa = Mesa{posicion::Int,capacidad::Comensales} deriving Show
data Restaurante = Restaurante {libres::[Mesa],ocupadas::[Mesa]} deriving Show

restauranteVacio = Restaurante [] []

insertarMesaLibre::Mesa->Restaurante->Restaurante
insertarMesaLibre mesa (Restaurante [] ocupadas) = Restaurante [mesa] ocupadas
insertarMesaLibre mesa (Restaurante libres ocupadas) = Restaurante (insertarMesaLibreAux mesa libres) ocupadas
    
insertarMesaLibreAux::Mesa->[Mesa]->[Mesa]
insertarMesaLibreAux mesa [] = [mesa]
insertarMesaLibreAux mesa (x:xs)
    | (capacidad mesa)>=(capacidad x) = x:(insertarMesaLibreAux mesa xs)
    | otherwise = x:mesa:xs

ocuparMesa::Restaurante->Comensales->Restaurante
ocuparMesa (Restaurante libres ocupadas) numPersonas  = Restaurante x y where (x,y) ocuparMesaAux libres ocupadas numPersonas