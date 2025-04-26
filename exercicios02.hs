{-Pertence 1-}
pertence x [] = False
pertence x (y:ys) = if x == y then True else pertence x ys 

{-Intercessao 2-}
intercessao [] ys = []
intercessao (x:xs) ys = if pertence x ys then x:intercessao xs ys else intercessao xs ys

{-Inverso 3-}
inverso [] = []
inverso (x:xs) = inverso xs++[x]

{-Ultimos n 4-}
nPrimeiros n [] = []
nPrimeiros 0 _ = []
nPrimeiros n (x:xs) = x:nPrimeiros (n-1) xs

nUltimos n xs = inverso (nPrimeiros n (inverso xs)) 

{-Soma2 5-}
soma2 _ [] = []
soma2 (x:xs) (y:ys) = x+y:soma2 xs ys

{-Pot2 6-}
pot 0 = []
pot n = 2^n:pot (n-1)

pot2 n = inverso(pot n) 

{-Intercalacao 7-}
intercalacao [] ys = ys
intercalacao xs [] = xs
intercalacao (x:xs) (y:ys) = if x <= y then x:intercalacao xs (y:ys) else y:intercalacao ys (x:xs)

{-Menor 8-}
menor1 n [] = n
menor1 n (x:xs) = if n > x then menor1 x xs else menor1 n xs
menor (x:xs) = menor1 x xs

{-RemoverElem 9-}

removerELem n [] = []
removerElem n (x:xs) = if n == x then xs else x:removerElem n xs

{-Ordenar 10-}
ordenar [] = []
ordenar xs = m:ordenar (removerElem m xs)
 where m = menor xs

{-Ins-}
ins1 n xs = xs++[n]
ins n xs = ordenar (ins1 n xs)

{-Enesimo-}
enesimo 1 (x:xs) = x
enesimo n (x:xs) = enesimo (n-1) xs 

{-Repitir-}
repitir 0 _ = []
repitir n m = m:repitir(n-1) m

{-Num String-}
numString 0 = []
numString n = numString(div n 10)++[int2char(rem n 10)]

int2char :: Int -> Char
int2char d = toEnum (d+48)

{-StringNum
stringNum [] = 0
stringNum [] = 

chart2int :: Char -> Int
char2int xs = fromEnum (xs-48)-}

{-Binario Inteiro-}


{-Inteiro Binario-}

auxiliar 0 = []
auxiliar n = (rem n 2):auxiliar(div n 2) 

listaString [] = []
listaString (x:xs) = int2char x:listaString xs

int2bin n = listaString(inverso(auxiliar n))
