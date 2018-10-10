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

distance::Coordenada->Coordenada->Double
distance (Coord x1 y1) (Coord x2 y2) = sqrt((x1-x2)^2+(y1-y2)^2)

--Recursive Types