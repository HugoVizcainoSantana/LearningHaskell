module Week3_Class1 where

--{
-- MergeSort in Haskell
--}
mergeSort::[Int]->[Int]
mergeSort [x] = [x] -- When it only has 1 element stop calling    
mergeSort array = merge(mergeSort firstHalfArray) (mergeSort secondHalfArray)
    where (firstHalfArray,secondHalfArray) = splitArray array
    
merge:: [Int]->[Int]->[Int]
merge xs [] = xs
merge [] ys = ys
merge (x1:xs) (y1:ys)
    |(x1<y1)    = x1:(merge xs (y1:ys))
    |otherwise  = y1:(merge (x1:xs) ys)

-- Aux funcs
splitArray::[Int]->([Int],[Int])
splitArray [] = ([],[])
splitArray array = (take half array,drop half array) 
    where half = length array `div` 2

-- Old Aux Funcs
-- firstHalf::[Int]->[Int]
-- firstHalf array = take (length array `div` 2) array 
--
-- secondHalf::[Int]->[Int]
-- secondHalf array = drop (length array `div` 2) array

-- Lambda time
invertList::[Int]->[Int]
invertList = foldr (\x lista -> lista++[x]) []

invertList2::[Int]->[Int]
invertList2 = foldl (\acum x -> x:acum) []

-- Remove all appearances of num in list
-- In: [1,3,4,12,4,52,35,4,62,6] 4
-- Out: [1,3,12,52,35,62,6]
removeNum::[Int]->Int->[Int]
removeNum array num = foldr(\x acum -> if not(x==num) then x:acum else acum) [] array

-- Insert element at end of list
insertEnd::[Int]->Int->[Int]
insertEnd array num = foldr(\x acum->x:acum) [num] array

-- Return # of appearances of element in list
-- In: [1,2,3,2,4,5,2,6,7] 2
-- Out: 3
numOfAppearences::[Int]->Int->Int
numOfAppearences array num = length (foldr(\x acum -> if (x==num) then x:acum else acum) [] array)


-- Functions as first class citizens!