{-Tamanho-}
tam [] = 0
tam (x:xs) = 1 + tam xs

{-Somatorio-}
soma [] = 0
soma (x:xs) = x + soma xs

{-Multiplicatorio-}
multi [] = 1
multi (x:xs) = x * multi xs

{-Maior-}
maior [x] = x
maior (x:xs) = if x > maior xs then x else maior xs