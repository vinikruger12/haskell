data Booleano = Verdadeiro|Falso deriving Show

nao Verdadeiro = Falso
nao Falso = Verdadeiro


Verdadeiro &&& Verdadeiro = Verdadeiro
a &&& b = Falso

Falso ||| Falso = Falso
a ||| b = Verdadeiro

Verdadeiro --> Falso = Falso
a --> b = Verdadeiro

(==) :: Bool -> Booleano -> Bool
False == Falso = True


data Semana = Dom | Seg | Ter | Qua | Qui | Sex | Sab deriving (Show, Eq, Ord)



proxDiaUtil Seg = Ter
proxDiaUtil Ter = Qua
proxDiaUtil Qua = Qui
proxDiaUtil Qui = Sex
proxDiaUtil _ = Seg

ehDiaUtil d = d > Dom && d < Sab

nEhDiaUtil d = d < Seg || d > Sex

data Dupla a b = Par a b deriving Show

primeiro (Par x y) = x
segundo (Par x y) = y

meuZip :: [a] -> [b] -> [Dupla a b]
meuZip _ [] = []
meuZip [] _ = []
meuZip (x:xs) (y:ys) = Par x y:meuZip xs ys


data Lista a = a :+: (Lista a)|Nil deriving Show

tam Nil = 0
tam (x :+: xs) = 1 + tam xs