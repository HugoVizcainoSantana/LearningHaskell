module Week4_Class2 where

type Punto=Double
data Coordenada = Coord{x::Punto,y::Punto} deriving Show

data Direccion = Norte | Este | Sur | Oeste deriving Show

-- Move Coordenada in specified direction
move::Direccion->Coordenada->Coordenada
move Norte c =Coord (x c) ((y c)+1)
move Este  c =Coord ((x c)+1) (y c)
move Sur   c =Coord (x c) ((y c)-1)
move Oeste c =Coord ((x c)-1) (y c)
