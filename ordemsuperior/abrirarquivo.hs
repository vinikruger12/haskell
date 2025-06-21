import GHC.Read (readField)
main = do putStrLn "Arquivos"
          arq <- getLine
          txt <- readFile arq
          putStrLn txt
          imprimir (lines txt)

imprimir :: [String] -> IO ()
imprimir [] = putStr "\n"
imprimir (l:ls) = do putStr (l++ "\n\n") 
                     imprimir ls
