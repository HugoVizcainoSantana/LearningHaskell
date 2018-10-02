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
-- In: 4 [1,3,4,12,4,52,35,4,62,6]
-- Out: [1,3,12,52,35,62,6]
removeNum::Int->[Int]->[Int]
removeNum num array = foldr(\x acum -> if not(x==num) then x:acum else acum) [] array

-- Insert element at end of list
insertEnd::[Int]->Int->[Int]
insertEnd array num = foldr(\x acum->x:acum) [num] array

-- Return # of appearances of element in list
-- In: [1,2,3,2,4,5,2,6,7] 2
-- Out: 3
numOfAppearences::[Int]->Int->Int
-- numOfAppearences array num = length (foldr(\x acum -> if (x==num) then x:acum else acum) [] array)
numOfAppearences array num = foldr(\x result -> if x==num then result+1 else result) 0 array

-- Remove multiples of num
-- In: [1,2,3,4,5,6,7,8,9,10] 3
-- Out: [1,2,4,5,7,8,10]
-- With foldr
removeMultiples::[Int]->Int->[Int]
removeMultiples array num = foldr(\x result -> if (x`mod`num == 0) then result else x:result) [] array

-- Filter list, left list even elements, right list odd elements
-- In: [1,2,3,4,5,6]
-- Out: [(1,3,5),(2,4,6)]
filterList::[Int]->([Int],[Int])
filterList array = (filter odd array, filter even array)

filterList2::[Int]->([Int],[Int])
filterList2 array = foldr(\x (rLeft,rRight) -> if (odd x) then (x:rLeft,rRight) else (rLeft,x:rRight)) ([],[]) array


-- Split list, result: 
-- Left side: Items that only appear once
-- Right side: duplicated Items (they have to appear only once)
exists = elem


filterListAppearsOnce::[Int]->([Int],[Int])
filterListAppearsOnce array = foldr(\x (r1,r2) -> 
                                if (x `exists` r2) then
                                    (r1,r2)
                                else
                                    if (x `exists` r1) then
                                        ((x `removeNum` r1),x:r2)
                                    else
                                        (x:r1,r2)
                                ) ([],[]) array


-- Functions as first class citizens!