import Data.Monoid (All(getAll))
main = (putStrLn "Qual o seu nome?" >> getLine) >>= (\nome -> putStrLn ("Olá, " ++ nome))

main2 = do putStrLn "Qual o seu nome?"
           nome <- getLine
           putStrLn ("Aló, " ++ nome)

idade = putStrLn "Qual a sua idade" >> getLine >>= (\idade1 -> putStrLn ("Então você tem " ++ idade1))