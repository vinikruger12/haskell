
ehVogal a = if a == 'a'|| a == 'e' || a == 'i' || a == 'o' || a == 'u' then True else False
semVogal = map (filter(not.ehVogal))
semVogal' [] = []
semVogal' (x:xs) = filter(not.ehVogal)x:semVogal' xs

trocar _ _ [] = []
trocar x y (s:sx) = if s == x then y:trocar x y sx else s:trocar x y sx

zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

sortAlfabetico xs = sortBy xs
aux (sortAlfabetico) n = if x == head(xs) then (x,n):aux xs (n+1) else aux xs n
sortCrescente (aux xs 1) = sortBy (\n1 n2 -> compare n1 n2)
maiorOcorrencia xs = head(reverse(sortaCrescente xs 1))