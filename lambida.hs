import Data.Char

{-- (\x -> x+1) n 

(\x -> \y -> x+y) n m

(\xy -> x + y) n m

(\x y w u -> x (y w) u) (\s z -> s (s z)) (\s z -> s (s (s z)))
--}

somar [] = []
somar (x:xs) = x+1:somar xs

dobrar [] = []
dobrar (x:xs) = x*2:dobrar xs

mapo f [] = []
mapo f (x:xs) = f x:mapo f xs

-- flip f x y = f y x
-- flip = \f x y -> f y x

somar1 = map (1+)
somar2 xs = map (1+) xs

dobrar1 = map (*2)
mapp f xs = [f x|x <- xs]

metade = mapo (/2)

potD2 = mapo (**2)


toUP = map toUpper 
toLO = map toLower

toAddList :: String -> [String] -> [String]
toAddList s = map (++ s)
