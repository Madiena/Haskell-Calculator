module Main where

import Data.List (intercalate)
import Parser.AbstractSyntax
  ( Definition (FunctionDef, VariableDef),
    Expression (..),
    OpCode (Add, Div, Mul, Pow, Sub),
  )
import Parser.Parser (parseReplInput)
import Parser.SymbolTable
  ( 
    Entry,
    SymbolTable,
    storeDefinition,
  )
import Parser.REPL ( ReplInput(Exp, Def) )

main :: IO ()
main = do
  repl ([] :: SymbolTable)

repl :: SymbolTable -> IO ()
repl table = do
  input <- getLine
  case parseReplInput input of
    Left err -> print err >> repl table
    Right entry ->
      case entry of
        Def definition -> do
          case updateTable (storeDefinition definition) table of
            Right newTable -> repl newTable
            Left err -> print err >> repl table
        Exp expression -> do
          case calculateExp expression table of
            Right erg -> print (show expression ++ " => " ++ show erg) >> repl table
            Left err -> print err >> repl table

updateTable :: Entry -> SymbolTable -> Either String SymbolTable
updateTable (name, VariableDef _ exp) table = foo1 (getEntrys name table) name table exp
updateTable (name, FunctionDef _ params body) table = foo2 (getEntrys name table) name table params body

foo1 :: SymbolTable -> String -> SymbolTable -> Expression -> Either String SymbolTable
foo1 [] name table exp = Right $ table ++ [(name, VariableDef name exp)]
foo1 [(_, FunctionDef {})] name _ _ = Left $ name ++ " ist bereits eine Funktion und kann keiner Zahl zugewisen werden"
foo1 [(_, VariableDef {})] name table exp = Right $ [x | x <- table, fst x /= name] ++ [(name, VariableDef name exp)] -- ersetze

foo2 :: SymbolTable -> String -> SymbolTable -> [String] -> Expression -> Either String SymbolTable
foo2 [] name table params body = Right $ table ++ [(name, FunctionDef name params body)]
foo2 [(_, VariableDef {})] name _ _ _ = Left $ name ++ " ist bereits eine Variable und kann keine Funktion zugewisen bekommen"
foo2 [(_, FunctionDef {})] name table params body = Right $ [x | x <- table, fst x /= name] ++ [(name, FunctionDef name params body)] -- ersetze

getEntrys :: Eq a => a -> [(a, b)] -> [(a, b)]
getEntrys x list = [(a, b) | (a, b) <- list, x == a]

calculateExp :: Expression -> SymbolTable -> Either String Double
calculateExp (Binary opCode exp1 exp2) table =
  case (calculateExp exp1 table, calculateExp exp2 table) of
    (Right erg1, Right erg2) -> Right (operator opCode erg1 erg2)
    (Left err, Left _) -> Left err
    (Left err, Right _) -> Left err
    (Right _, Left err) -> Left err
calculateExp (Number d) _ = Right d
calculateExp (Var name) table =
  case [(ident, def) | (ident, def) <- table, ident == name] of
    [] -> Left $ "Keine Variable " ++ name ++ " vorhanden"
    [(_, VariableDef _ expr)] -> calculateExp expr table
    [(_, FunctionDef name params body)] -> Left $ name ++ "(" ++ intercalate ", " params ++ ")=" ++ show body
    [x, y] -> Left "Bitte benenne die Parameter der verschachtelten Funktionen verschieden"
    _ -> Left "ein anderer Fehler ist aufgetreten."
calculateExp (Application name arguments) table =
  case [(ident, def) | (ident, def) <- table, ident == name] of
    [] -> Left $ "Keine Funktion " ++ name ++ " vorhanden"
    [(_, FunctionDef funcName params bodyExp)] -> calculateExp bodyExp $ table ++ paramsArgumentsMap params arguments
    _ -> Left $ name ++ " ist keine Funktion"

paramsArgumentsMap :: [String] -> [Expression] -> SymbolTable
paramsArgumentsMap params arguments = map (\(param, arg) -> (param, VariableDef param arg)) (zip params arguments)

operator :: OpCode -> Double -> Double -> Double
operator Add = (+)
operator Sub = (-)
operator Mul = (*)
operator Div = (/)
operator Pow = (**)

