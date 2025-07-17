


compDuplas [] = []
compDuplas ((a,b):xs) = (b,a):compDuplas xs

zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

ehVogal a = if a == 'a' || a == 'e' || a == 'i' || a == 'o' || a == 'u' then True else False

semVogal [] = []
semVogal (x:xs) = filter (not.ehVogal) x: semVogal xs

compTrios [] = []
compTrios ((a,b,c):xs) = (c,b,a):compTrios xs

soVogal [] = []
soVogal (x:xs) = filter (ehVogal) x:soVogal xs


compDuplas' = map (\(a,b) -> (b,a))

semNenhumaVogal :: [String] -> [String]
semNenhumaVogal xs = filter (not . any ehVogal) xs

contaVogal :: String -> Int
contaVogal = foldl (\acc x -> if ehVogal x then acc + 1 else acc) 0


somaDuplas xs = foldr (\(a,b) (accA,accB) -> (accA + a, accB + b)) (0,0) xs

somaTripos xs = foldr (\(a,b,c) (accA,accB,accC) -> (accA + a, accB + b, accC + c)) (0,0,0) xs

