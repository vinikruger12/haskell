primeiro (x,y) = x

segundo (x,y) = y

primeiros [] = []
primeiros (x:xs) = primeiro x:primeiros xs

segundos [] = []
segundos (y:ys) = segundo y:segundos ys

procura n [] = " "
procura n (x:xs) = if n == primeiro x then segundo x else procura n xs

{-Triplas-}

tabuada = tabuado 1 1
tabuado 10 _ = []
tabuado x y = if y == 9 then (x, y, x*y):tabuado (x+1) 1 else (x,y,x*y):tabuado x (y+1)

{-Usando teoria dos conjuntos-}
tabuadaa = [(x,y,x*y)|x <- [1..9], y <- [1..9]]


{-Primos so pra relembrar-}
primo :: Integral t => t -> Bool
primo n = primod n (n-1)
primod :: Integral t => t -> t -> Bool
primod n 1 = True
primod n d = if rem n d == 0 then False else primod n (d-1) 
