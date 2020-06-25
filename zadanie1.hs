import Data.List

--SUMA--
mysum ::(Num a) => [a] -> a
mysum = foldl (+) 0

mysum2 ::(Num a) => [a] -> a
mysum2 xs = foldl(\acc x -> acc + x) 0 xs

--PRODUCT--
myproduct ::(Num a) => [a] -> a
myproduct = foldl (*) 1

--REVERSE--
myreverse :: [a] -> [a]
myreverse xs = foldl (\acc x -> x : acc) [] xs  -- : służy do odwracania listy 

--możemy też użyć wersji bez 'xs', oszczędzamy sobie trochę czasu
--myreverse :: [a] -> [a]
--myreverse = foldl (\acc x -> x : acc) []  

--AND--
myand :: [Bool] -> Bool
myand = foldl(\acc x -> acc && x) True

--OR--
myor :: [Bool] -> Bool
myor = foldl(\acc x -> acc || x) False

--HEAD--
myhead :: [a] -> a  
myhead = foldr1 (\x y -> x)  

--LAST--
mylast :: [a] -> a  
mylast = foldl1 (\y x -> x)  
