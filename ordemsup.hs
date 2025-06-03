par x = rem x 2 == 0
impar = not<.>par

filtero _ [] = []
filtero p (x:xs) = if p x then x:filtero p xs else filtero p xs

(<.>) :: (t1 -> t2) -> (t3 -> t1) -> t3 -> t2
g <.> f = \x -> g (f x)

g &&# f = (g + f) * 2
g @ f = (g - f) * 200