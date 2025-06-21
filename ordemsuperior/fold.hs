
meuFoldr :: (t1 -> t2 -> t2) -> t2 -> [t1] -> t2
meuFoldr f b [] = b
meuFoldr f b (x:xs) = f x (meuFoldr f b xs)

mapo :: (t -> a) -> [t] -> [a]
mapo f [] = []
mapo f (x:xs) = f x:mapo f xs

filtero :: (a -> Bool) -> [a] -> [a]
filtero p [] = []
filtero p (x:xs) = if p x then x:filtero p xs else filtero p xs

allo [] = True
allo (x:xs) = x && allo xs

alloFoldr xs = meuFoldr (&&) True xs

