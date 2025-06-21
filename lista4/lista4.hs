import Data.List (sortBy, groupBy)
import Data.Function (on)

type Doc = String
type Line = String
type Word' = String


numLines xs = numLineso 1 (lines xs)
numLineso n [] = []
numLineso n (x:xs) = (n,x):numLineso (n+1) xs

allNumWords :: [(Int, Line)] -> [(Int, Word')]
allNumWords [] = []
allNumWords ((n,l):xs) = map (\w -> (n, w)) (words l) ++ allNumWords xs 

-- essa função recebe n e l, sendo respectivamente:
-- n: numero da linha
-- l: string da linha
-- após isso, usa-se o map para pegar a palavra como parâmetro
-- e retornar uma tupla com o numero e a palavra
-- se o map receber: [(1,"batatinha eh bom")]
-- ele retornará: [(1,"batatinha"),(1,"eh"),(1,"bom")]
-- após isso ele concatenará com o restante da lista


-- d) Ordenar alfabeticamente as ocorrências de palavras no texto

sortLs :: [(Int, Word')] -> [(Int, Word')]
sortLs = sortBy (\(_, w1) (_, w2) -> compare w1 w2)

-- a funcao sortBy receberá como parâmetro a comparação de
-- uma funcao anonima que recebe o segundo elemento de
-- duas tuplas, após isso, ordenará por palavra


-- e) Juntar as várias ocorrências de cada palavra, produzindo, para cada palavra, a lista dos
-- números das linhas em que a palavra ocorre:

almalgamate :: [(Int, Word')] -> [([Int], Word')]
almalgamate xs = map (\ws -> (map fst ws, snd (head ws))) (groupBy (\x y -> snd x == snd y) (sortLs xs))
-- primeiro separar as funcoes por partes
-- map: aplica a função (\ws -> (map fst ws, snd (head ws))) na lista de palavras agrupadas

-- (\ws -> (map fst ws, snd (head ws)))
-- a funcao recebe: [(3,"banana"),(7,"banana")]
-- map fst ws         --> [3, 7]
-- snd (head ws)      --> "banana"
-- Resultado final    --> ([3, 7], "banana")

-- essa primeira funcao só é possível por conta da segunda funcao do groupBy
-- que primeiro ordena com sortLs e depois agrupa em uma matriz:
-- lista ordenada: [(1, "banana"), (3, "banana"), (2, "carambola"), (4, "manga")]
-- matriz do groupBy:

-- [
--   [(1, "banana"), (3, "banana")],
--   [(2, "carambola")],
--   [(4, "manga")]
-- ]


-- f) Eliminar, da lista de números de linhas em que cada palavra ocorre, as repetições de um
-- mesmo número de linha:

shorten :: [([Int],Word')] -> [([Int],Word')]
shorten = map (\(ns, w) -> (unique ns, w))

unique = foldr (\x seen -> if x `elem` seen then seen else x : seen) []

-- pega a lista de linhas onde o numero se repete e aplica o unique auxiliar

