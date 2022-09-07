module Main where

import Parser.Parser

main :: IO()
main = do
    str <- getLine
    case parseFunction str of 
        Left err -> putStrLn "err"
        Right suc -> print (compileToJS suc)
    main
        
