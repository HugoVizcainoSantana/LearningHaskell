--{
-- MergeSort in Haskell
--}
mergeSort::[Int]->[Int]
mergeSort [x] = [x] -- When it only has 1 element stop calling    

mergeSort array = merge(mergeSort (firstHalf array)) (mergeSort (secondHalf array))
    
merge:: [Int]->[Int]->[Int]
merge xs [] = xs
merge [] ys = ys
merge (x1:xs) (y1:ys)
    |(x1<y1)    = x1:(merge xs (y1:ys))
    |otherwise  = y1:(merge (x1:xs) ys)

-- Aux funcs
firstHalf::[Int]->[Int]
firstHalf array = take (length array `div` 2) array 

secondHalf::[Int]->[Int]
secondHalf array = drop (length array `div` 2) array
