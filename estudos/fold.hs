meuFoldr f b [] = b
meuFoldr f b (x:xs) = f x (meuFoldr f b xs)