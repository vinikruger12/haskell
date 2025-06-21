{-Maximo divisor comum-}

{- Primeira forma -}

mdc a b = if a == b then a else if a > b then mdc (a-b) b else mdc a (b-a) 

{-Segunda forma-}

mdc1 a b | a == b = a
         | a > b = mdc (a-b) b
         | otherwise = mdc a (b-a)