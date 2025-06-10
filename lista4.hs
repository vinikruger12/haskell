type Doc = String
type Line = String
type Word' = String


numLines xs = numLineso 1 (lines xs)
numLineso n [] = []
numLineso n (x:xs) = (n,x):numLineso (n+1) xs

allNumWords ((n,l):xs) = allNumWordo 1 (words l)
allNumWordo n [] = []
allNumWordo n (x:xs) = (words(numLines x)):allNumWordo xs 

