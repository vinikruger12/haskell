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
allNumWords ((x,l):xs) = zip (repeat x) (words l) ++ allNumWords xs

remover3 :: [(Int, Word')] -> [(Int,Word')]
remover3 = filter (\(_, w) -> length w > 3)

{-D-}

sortLs :: [(Int, Word')] -> [(Int, Word')]
sortLs = sortBy (\(_, w1) (_, w2) -> compare w1 w2)
 

{-E-}

almalgamate :: [(Int, Word')] -> [([Int], Word')]
almalgamate xs = map (\ws -> (map fst ws, snd (head ws))) (groupBy (\x y -> snd x == snd y) (sortLs xs))

    {- SORT Ordena a lista de pares (linha, palavra) por palavra.-}
    {-groupBy Agrupa os pares que têm a mesma palavra.-}
    
    {-Para cada grupo (ex: [(1,"bola"), (4,"bola")]), cria um par assim:
    a lista de números de linha: map fst ws → [1, 4]
    a palavra do grupo: snd (head ws) -> "bola"-}

{-F-}

shorten :: [([Int],Word')] -> [([Int],Word')]
shorten = map (\(ns, w) -> (nUnico ns, w))

nUnico = foldr (\x visto -> if x `elem` visto then visto else x : visto) []


makeindex :: Doc -> [([Int], Word')]
makeindex txt = almalgamate . sortLs . remover3 . allNumWords . numLines $ txt


formatIndex :: [([Int], Word')] -> String
formatIndex xs = unlines [w ++ " - " ++ show ns | (ns, w) <- xs]


main :: IO ()
main = do
    putStr "Arquivos: "
    arquivo <- getLine
    txt <- readFile arquivo
    putStr $ formatIndex (makeindex txt)
