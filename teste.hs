somaPares a 
 |a <= 0 = 0 
 |rem a 2 /= 0 = somaPares (a-1)
 |otherwise = a + somaPares (a-2)


somaPot2m m 0 = m
somaPot2m m n = 2**n * m + somaPot2m m (n-1)

primo a
 |a <= 1 = False
 |a == 2 = True
 |rem a 2 == 0 = False
 |otherwise = (procurando a (ceiling(sqrt(fromIntegral a))) 3)

procurando a b c
 |c > b = True
 |rem a c == 0 = False
 |otherwise = procurando a b (c+2)

seriePI n = termos n 1 0 1
termos n denominador soma sinal 
 |(4/denominador) > (4/n) = termos n (denominador+2) (soma + sinal*(4/denominador)) (-sinal)
 |otherwise = soma