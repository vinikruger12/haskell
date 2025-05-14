import Data.Time.Format.ISO8601 (yearFormat)
primo n = prim0 n (n-1)
prim0 n 1 = True
prim0 n d = if rem n d == 0 then False else prim0 n (d-1)

primod = [(x)|x <- [2..1000],primo x]
numero = [n|n<- [1..10000]]


impares [] = []
impares (x:xs) = if rem x 2 == 0 then impares xs else x:impares xs

pares [] = []
pares (x:xs) = if rem x 2 == 0 then x:pares xs else pares xs

primeiro (x,y) = x
primeiros [] = []
primeiros (x:xs) = primeiro x:primeiros xs

segundo (x,y) = y
segundos [] = []
segundos (x:xs) = segundo x:segundos xs