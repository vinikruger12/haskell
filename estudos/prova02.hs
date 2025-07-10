
compDuplas [] = []
compDuplas ((a,b):xs) = (b,a):compDuplas xs

compDuplas' = map (\(a,b) -> (b,a))

zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

ehVogal a = if a == 'a' || a == 'e' || a == 'i' || a == 'o' || a == 'u' || a == 'A' || a == 'E' || a == 'I' || a == 'O' || a == 'U' then True else False

semVogal [] = []
semVogal (x:xs) = filter (not.ehVogal) x :semVogal xs

inverteTRIPLAS = map (\(a,b,c) -> (c,b,a))

soMenteOsPrimeiros = map (\(a,b,c) -> a)

soMenteOsPrimeiros' [] = []
soMenteOsPrimeiros' ((a,b,c):xs) = a:soMenteOsPrimeiros' xs

meuFoldl f b [] = b
meuFoldl f b (x:xs)  = f x (meuFoldl f b xs)

soVogal [] = []
soVogal (x:xs) = filter (ehVogal)x:soVogal xs

semVogal' = map (filter(not.ehVogal))

par a = if rem a 2 == 0 then True else False
soPar = filter(par)
soImpar = filter(not.par)


associados [] _ = []
associados (x:xs) n = (x,n):associados xs (n+1)
aSS xs = associados xs 1

semVogalo = map (filter(not.ehVogal))