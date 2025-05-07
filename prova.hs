impares [] = []
impares (x:xs) = if rem x 2 /= 0 then x:impares xs else impares xs

remover n [] = []
remover n (x:xs) = if n == x then remover n xs else x:remover n xs

todos [] = True
todos (x:xs) = if x == False then False else todos xs

segundo (x,y) = y
segundos [] = []
segundos (x:xs) = segundo x:segundos xs