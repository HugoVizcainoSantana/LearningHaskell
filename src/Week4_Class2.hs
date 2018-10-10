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
data Tree a = EmptyTree | Tree a (Tree a) (Tree a) deriving Show

--for Testing
treeTest = Tree 45 (Tree 27 (Tree 22 EmptyTree EmptyTree) EmptyTree) (Tree 10 EmptyTree(Tree 8 EmptyTree (Tree 5 (Tree 245 EmptyTree EmptyTree) EmptyTree)))

--Count nodes in Tree
nodesCount::Tree a->Int
nodesCount EmptyTree = 0
nodesCount (Tree node left right) = 1+(nodesCount left)+(nodesCount right)

--Count leafs in Tree
leafCount::Tree a->Int
leafCount EmptyTree = 0
leafCount (Tree _ EmptyTree EmptyTree) = 1
leafCount (Tree _ left right)= leafCount left + leafCount right

--Height of Tree
treeHeight::Tree a->Int
treeHeight EmptyTree = 0
--Unnecesary treeHeight (Tree _ EmptyTree EmptyTree) = 1
treeHeight (Tree _ left right)= 1 + max (treeHeight left) (treeHeight right)

type BST a= Tree a
--Insert in PROPER ORDER, AS A BINARY SEARCH TREE WOULD
insertInBST::(Ord a)=> BST a-> a -> BST a
insertInBST EmptyTree val = Tree val EmptyTree EmptyTree
insertInBST (Tree node left right) val 
	|(val<node) = (Tree node (insertInBST left val) right)
	|otherwise = (Tree node left (insertInBST right val))
	
--Traverse Trees
prefixTraversal::Tree a ->  [a]
prefixTraversal EmptyTree = []
prefixTraversal (Tree node left right) = node:(prefixTraversal left ++ prefixTraversal right)

infixTraversal::Tree a ->   [a]
infixTraversal EmptyTree = []
infixTraversal (Tree node left right) = prefixTraversal left ++[node]++ prefixTraversal right

postfixTraversal::Tree a -> [a]
postfixTraversal EmptyTree = []
postfixTraversal (Tree node left right) = prefixTraversal left ++ prefixTraversal right ++[node]
	
--Build Tree from array, depending on its order

	
	


