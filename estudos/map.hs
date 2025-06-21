import Data.Char (toLower, toUpper)
map' f [] = []
map' f (x:xs) = f x : map' f xs


toLo = map toLower
toup = map toUpper