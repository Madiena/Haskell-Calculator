module Main where

import Parser

main :: IO()
main = do
    str <- getLine
    case parseFunction str of 
        Left err -> putStrLn "err"
        Right suc -> print suc
    main
        
