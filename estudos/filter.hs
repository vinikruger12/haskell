filter' _ [] = []
filter' p (x:xs) = if p x then x:filter' p xs else filter' p xs

par x = rem x 2 == 0
impar = not.par

g &$& f = (g / f) + (g * f) + (f + g) + (f - g)