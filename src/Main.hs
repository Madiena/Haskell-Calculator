module Main where

import Parser.Parser
import Parser.ZeroCrossings

main :: IO()
main = do
    str <- getLine
    case parseFunction str of 
        Left err -> putStrLn (show err)
        Right suc ->
            print (compileToJS suc)
            >> print (calculate (returnExpressionFromDef suc)) 
            -- >> print (calculateZeroPoints $ returnExpressionFromDef suc)
    main