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

{-StringNum-}
aux _ (-1) = 0
aux (x:xs) b = ((char2int x)*10^b) + aux xs (b-1)
stringNum [] = 0
stringNum a = aux a ((length a)-1)

char2int :: Char -> Int
char2int xs = fromEnum (xs) - 48

{-Binario Inteiro-}

bin2int [] = 0
bin2int (x:xs) = ((char2int x)*2^(length (x:xs)-1)) + bin2int xs


{-Inteiro Binario-}

auxiliar 0 = []
auxiliar n = (rem n 2):auxiliar(div n 2) 

listaString [] = []
listaString (x:xs) = int2char x:listaString xs
int2bin n = listaString(inverso(auxiliar n))

{-letras minusculas-}

char2i :: Char -> Int
char2i xs = fromEnum (xs)
int2c :: Int -> Char
int2c d = toEnum d
minusculas [] = []
minusculas (x:xs) =let m = (char2i x)in if m >=65 && m<=90 then (int2c (m+32)):minusculas xs else x:minusculas xs


{-letras maiusculas-}


maiusculas [] = []
maiusculas (x:xs) =let m = (char2i x)in if m >=97 && m<=122 then (int2c (m-32)):maiusculas xs else x:maiusculas xs
