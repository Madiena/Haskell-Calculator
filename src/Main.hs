module Main where

import Parser.Parser
import Parser.ZeroCrossings
import Parser.SymbolTable
import Parser.AbstractSyntax (Definition)

main :: IO()
main = do
    repl [] 
    


repl :: SymbolTable -> IO ()
repl table = do
    print table
    str <- getLine
    case parseFunction str of 
        Left err -> print err
        Right suc -> do
            print (compileToJS suc)
            repl (storeDefinition suc ++ table)
            {- case semanticAnalysis (table ++ storeDefinition suc) of
                Right t -> repl t
                Left err -> do
                    print err 
                    repl table -}

{-
--wenn eine Varibale wiederholt definiert wird, wird der Eintrag in der Tabelle geändert
semanticAnalysis :: SymbolTable -> Either String SymbolTable 
--semanticAnalysis (FunctionDef {funcName, funcParams, funcBody} ) table = 
semanticAnalysis [] = Right []
semanticAnalysis (head:[tail]) = case semanticAnalysis tail of
    Left err -> Left err
    Right succ -> check head 
-}


--[a = 6, c=a-2, a=1,  b = a+1]
-- z=u+1
-- z = f(3)
-- gibt es schon einen eintrag mit gleichem namen => ändere das in tabelle
-- alle (Variablen/Applikation) durchgehen, checken ob es die gibt


{-
variablesInExpression :: Expression -> [Definition]
variablesInExpression _ = undefined

-}


{-

z=1

f(x)=1*x

z(x)=x*x

f(5)

-}
