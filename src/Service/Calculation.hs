{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Service.Calculation (calculateExp) where

import Data.List (intercalate)
import Service.AbstractSyntax
  ( Definition (FunctionDef, VariableDef),
    Expression (..),
    OpCode (Add, Div, Mul, Pow, Sub),
  )
import Service.SymbolTable (SymbolTable)

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
    [_, _] -> Left "Bitte benenne die Parameter der verschachtelten Funktionen verschieden"
    _ -> Left "ein anderer Fehler ist aufgetreten."
calculateExp (Application name arguments) table =
  case [(ident, def) | (ident, def) <- table, ident == name] of
    [] -> Left $ "Keine Funktion " ++ name ++ " vorhanden"
    [(_, FunctionDef _ params bodyExp)] -> calculateExp bodyExp $ table ++ paramsArgumentsMap params arguments
    _ -> Left $ name ++ " ist keine Funktion"

paramsArgumentsMap :: [String] -> [Expression] -> SymbolTable
paramsArgumentsMap params arguments = map (\(param, arg) -> (param, VariableDef param arg)) (zip params arguments)

operator :: OpCode -> Double -> Double -> Double
operator Add = (+)
operator Sub = (-)
operator Mul = (*)
operator Div = (/)
operator Pow = (**)
