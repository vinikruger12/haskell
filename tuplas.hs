primeiro (x,y) = x

segundo (x,y) = y

primeiros [] = []
primeiros (x:xs) = primeiro x:primeiros xs

segundos [] = []
segundos (y:ys) = segundo y:segundos ys

procura n [] = " "
procura n (x:xs) = if n == primeiro x then segundo x else procura n xs