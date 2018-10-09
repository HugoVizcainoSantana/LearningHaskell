module Week4_Class1 where

import Data.Char;
import Debug.Trace

-- Given an input array, show its insertion sequence, starting from an empty array
insertionSequence::[a]->[[a]]
insertionSequence [] = [[]]
-- With foldl
insertionSequence array = foldl(\result x->result++[last result++[x]]) [[]] array

-- Get N biggest numbers in array
getBiggestNumbers::[Int]->Int->[Int]
getBiggestNumbers array n = getBiggestNumbersAux array n []

getBiggestNumbersAux::[Int]->Int->[Int]->[Int]
getBiggestNumbersAux [] _ result = result
getBiggestNumbersAux array 0 result = foldl(\res x-> swapByLowerIfGreater x res) result array
getBiggestNumbersAux array n result = getBiggestNumbersAux (drop n array) 0 (take n array)

-- If greater than the lowest number in array, it swaps it
swapByLowerIfGreater::Int->[Int]->[Int]
swapByLowerIfGreater n array = if low<n then (swap low n array) else array where low = lowestFromArray array
                                                    
swap::Int->Int->[Int]->[Int]
swap n1 n2 (x:xs) = if (n1==x) then n2:xs else x:(swap n1 n2 xs)
swap _ _ [] = []          
                    
lowestFromArray::[Int]->Int
lowestFromArray (x:xs) = lowestFromArrayAux xs x
lowestFromArrayAux::[Int]->Int->Int
lowestFromArrayAux (x:xs) return = if x<return then lowestFromArrayAux xs x else lowestFromArrayAux xs return
lowestFromArrayAux [] return = return

-- Types and Clases (Data)
type Numerador = Float
type Denominador = Float
data Fraccion = Numerador :/ Denominador deriving Show

areEquivalent::Fraccion->Fraccion->Bool
areEquivalent (num1:/den1) (num2:/den2) = (num1/den1 == num2/den2)