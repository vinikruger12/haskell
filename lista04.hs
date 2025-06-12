import Data.List (sortBy, groupBy)
import Data.Function (on)

type Doc = String
type Line = String
type Word' = String

{-B-}

numLines :: Num t => String -> [(t, String)]
numLines xs = numLineso 1 (lines xs)
numLineso n [] = []
numLineso n (x:xs) = (n,x):numLineso (n+1) xs

{-C-}

allNumWords [] = []
allNumWords ((n,l):xs) = map (\w -> (n, w)) (words l) ++ allNumWords xs

{-D-}

sortLs :: [(Int, Word')] -> [(Int, Word')]
sortLs = sortBy (\(_, w1) (_, w2) -> compare w1 w2)

{-E-}

almalgamate :: [(Int, Word')] -> [([Int], Word')]
almalgamate xs = map (\ws -> (map fst ws, snd (head ws))) (groupBy (\x y -> snd x == snd y) (sortLs xs))

{-F-}

shorten :: [([Int],Word')] -> [([Int],Word')]
shorten = map (\(ns, w) -> (unique ns, w))

unique = foldr (\x seen -> if x `elem` seen then seen else x : seen) []

