{-Exercicio Triangulo-}
ehTriangulo a b c = if a + b <= c 
 then False 
  else if a + c <= b 
   then False 
    else if b + c <= a 
     then False else True


{-Equilatero, Isoceles, Escaleno-}
tipoTriangulo a b c = if a == b && a == c && c == b 
 then putStrLn "equilatero"
  else if a == b || a == c || b == c 
   then putStrLn "isoceles" 
    else putStrLn "escaleno"

{-Nao eh ou eh-}
triangulo a b c = if ehTriangulo a b c == True 
  then tipoTriangulo a b c 
   else putStrLn "não eh um triangulo"

{-Soma Pares-}
somaPares a = if a <= 0 then 0 
 else if rem a 2 /= 0 
  then somaPares (a-1) 
   else a + somaPares (a-2) 

{-Soma potencia-}
somaPot2m m 0 = m
somaPot2m m n = 2**n * m + somaPot2m m (n-1)

{-Primos-}
primos a
 |a <= 1 = False
 |a == 2 = True
 |rem a 2 == 0 = False
 |otherwise = (procura a (ceiling(sqrt(fromIntegral a))) 3)
 
procura a b c
 |c > b = True
 | rem a c == 0 = False
 |otherwise = procura a b (c+2)


{-Pi-}
seriePI n = termos n 1 0 1
termos n denominador soma sinal
 | (4/denominador) > (4/n) = termos n (denominador+2) (soma + sinal * (4 / denominador)) (-sinal)
 | otherwise = soma
