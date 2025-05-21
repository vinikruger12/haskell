{-Acho que tiro 6-}

{-1 certo-}
nprimeiros _ [] = []
nprimeiros 0 xs = []
nprimeiros n (x:xs) = x:nprimeiros (n-1) xs

{-2-}
inverso [] = []
inverso (x:xs) = inverso xs ++ [x]
ultimo xs = head (inverso xs)

{-3 certo-} 
replicar1 0 x = []
replicar1 n x = x:replicar1 (n-1) x
replicar _ [] = []
replicar n (x:xs) = replicar1 n x++replicar n xs

{-4 certo-}
aprim _ [] = []
aprim 0 xs = xs
aprim n (x:xs) = aprim (n-1) xs
 
fatiar n m xs = aprim n (nprimeiros (m+1) xs)

{-5 certo-}
primeiro (x,y) = x
segundo (x,y) = y
menores [] = []
menores (x:xs) = if primeiro x < segundo x then x:menores xs else menores xs 