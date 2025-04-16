{-Pertence-}
pertence x [] = False
pertence x (y:ys) = if x == y then True else pertence x ys 

{-Intercessao-}
intercessao [] ys = []
intercessao (x:xs) ys = if pertence x ys then x:intercessao xs ys else intercessao xs ys

{-Inverso-}
inverso [] = []
inverso (x:xs) = inverso xs++[x]

{-Ultimos n-}
nPrimeiros n [] = []
nPrimeiros 0 _ = []
nPrimeiros n (x:xs) = x:nPrimeiros (n-1) xs

nUltimos n xs = inverso (nPrimeiros n (inverso xs)) 