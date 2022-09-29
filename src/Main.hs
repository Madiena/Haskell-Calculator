module Main where

import Parser.Parser
import Parser.ZeroCrossings
import Parser.SymbolTable
import Parser.AbstractSyntax
    ( OpCode(Pow, Add, Sub, Mul, Div),
      Expression(..),
      Definition(VariableDef, FunctionDef) ) 
import Data.List

main :: IO()
main = do
    repl ([] :: SymbolTable)
    


repl :: SymbolTable -> IO ()
repl table = do
    -- print table
    input <- getLine
    case parseReplInput input of 
        Left err -> print err >> repl table
        Right entry -> 
            
            case entry of 
                Def definition -> do
                    case  updateTable (storeDefinition definition) table of 
                        Right newTable -> repl newTable
                        Left err -> print err >> repl table
                Exp expression -> do 
                    case calculateExp expression table of
                        Right erg -> print (show expression ++ " => " ++ show erg) >> repl table
                        Left err -> print err >> repl table

{-
updateTable :: Entry -> SymbolTable -> Either String SymbolTable 
updateTable (name, VariableDef _ exp) table 
    | getEntrys name table == [] = Right $ table ++ [(name, VariableDef name exp)]
    | getEntrys name table == [(_ , FunctionDef {})] =  Left $ name ++ " ist bereits eine Funktion und kann keiner Zahl zugewisen werden" 
    | getEntrys name table == [(_, VariableDef {})] = Right $ [x | x <- table, snd x /= name] ++ [(name, VariableDef name exp)] -- ersetze
updateTable (name, FunctionDef _ params body) table
    | getEntrys name table == [] = Right $ table ++ [(name, FunctionDef name params body)]
    | getEntrys name table == [(_, VariableDef {})] = Left $ name ++ " ist bereits eine Variable und kann keine Funktion zugewisen bekommen"
    | getEntrys name table == [(_, FunctionDef {})] = Right $ [x | x <- table, snd x /= name] ++ [(name, FunctionDef name params body)] -- ersetze
-}
updateTable :: Entry -> SymbolTable -> Either String SymbolTable 
updateTable (name, VariableDef _ exp) table =  foo1 (getEntrys name table) name table exp
updateTable (name, FunctionDef _ params body) table= foo2 (getEntrys name table) name table params body 
 
foo1 :: SymbolTable -> String -> SymbolTable -> Expression -> Either String SymbolTable 
foo1 [] name table exp= Right $ table ++ [(name, VariableDef name exp)]
foo1 [(_ , FunctionDef {})] name _ _ =  Left $ name ++ " ist bereits eine Funktion und kann keiner Zahl zugewisen werden" 
foo1 [(_, VariableDef {})] name table exp= Right $ [x | x <- table, fst x /= name] ++ [(name, VariableDef name exp)] -- ersetze

foo2 :: SymbolTable -> String -> SymbolTable -> [String] -> Expression -> Either String SymbolTable 
foo2 [] name table params body= Right $ table ++ [(name, FunctionDef name params body)]
foo2 [(_, VariableDef {})] name _ _ _= Left $ name ++ " ist bereits eine Variable und kann keine Funktion zugewisen bekommen"
foo2 [(_, FunctionDef {})] name table params body= Right $ [x | x <- table, fst x /= name] ++ [(name, FunctionDef name params body)] -- ersetze


getEntrys :: Eq a => a -> [(a,b)] -> [(a,b)]
getEntrys x list = [(a,b) | (a,b) <- list , x == a]
{-updateTable (name, def) table = case [(name', def') | (name', def') <- table, name' == name] of 
    [] ->  table ++ [(name, def)]
    _:_ -> [(name', def') | (name', def') <- table, name' /= name] ++ [(name, def)]
-}

calculateExp :: Expression -> SymbolTable -> Either String Double
calculateExp (Binary opCode exp1 exp2) table = 
    case (calculateExp exp1 table, calculateExp exp2 table) of 
        (Right erg1, Right erg2) -> Right (operator opCode erg1 erg2)
        (Left err, Left _) ->   Left err
        (Left err, Right _) ->   Left err
        (Right _, Left err) ->   Left err
calculateExp (Number d) _ = Right d
calculateExp (Var name) table = 
    case [(ident, def)| (ident, def) <- table , ident == name] of 
        [] -> Left $ "Keine Variable " ++ name ++ " vorhanden"
        [(_, VariableDef _ expr)] -> calculateExp expr table
        [(_, FunctionDef name params body )] -> Left $ name ++ "(" ++ intercalate ", " params ++ ")=" ++  show body
        [x,y] -> Left "Bitte benenne die Parameter der verschachtelten Funktionen verschieden"
        _ -> Left "ein anderer Fehler ist aufgetreten."
calculateExp (Application name arguments) table=
    case [(ident, def)| (ident, def) <- table , ident == name] of 
        [] -> Left $ "Keine Funktion " ++ name ++ " vorhanden"
        [(_, FunctionDef funcName params bodyExp)] -> calculateExp bodyExp $ paramsArgumentsMap params arguments
        _ -> Left $ name ++ " ist keine Funktion"

paramsArgumentsMap :: [String] -> [Expression] -> SymbolTable
paramsArgumentsMap params arguments = map (\(param, arg)   -> (param , VariableDef param arg)) (zip   params arguments) 

operator :: OpCode -> Double -> Double -> Double
operator Add = (+)
operator Sub = (-)
operator Mul = (*)
operator Div = (/)
operator Pow = (**)
-- operator _ = undefined


--compute :: SymbolTable -> Either String SymbolTable


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
