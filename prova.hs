import System.Win32 (COORD(yPos))
foo a b = if a == b then b else if a > b then foo (a-b) b else foo a (b-a)

impares [] = []
impares (x:xs) = if rem x 2 /= 0 then x:impares xs else impares xs 

pares [] = []
pares (x:xs) = if rem x 2 == 0 then x:pares xs else pares xs

remover n [] = []
remover n (x:xs) = if x == n then remover n xs else x:remover n xs

removerum n [] = []
removerum n (x:xs) = if n==x then xs else x:removerum n xs

todos [] = True
todos (x:xs) = if x == False then False else todos xs

segundo (x,y) = y
segundos [] = []
segundos (x:xs) = segundo x:segundos xs

primeiro (x,y) = x
primeiros [] = []
primeiros (x:xs) = primeiro x:primeiros xs
