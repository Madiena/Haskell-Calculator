{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use second" #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Service.Calculation (calculateExp) where

import Data.List (intercalate)
import Service.AbstractSyntax
  ( Definition (FunctionDef, VariableDef),
    Expression (..),
    OpCode (Add, Div, Mul, Pow, Sub),
  )
import Service.SymbolTable (SymbolTable)
import Text.Printf (printf)
import Service.SymbolTable (SymbolTable)
import Data.Either

{-
    wertet eine Expression im Kontext einer Symboltabelle aus zu entweder einer Zahl oder einer Nachricht die in der REPL ausgegeben werden kann
-}
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
    [(_, FunctionDef name params body)] -> Left $ name ++ "(" ++ intercalate ", " params ++ ")=" ++ show body ++", zeropoints: " ++ show (calculateZeroPoints table (FunctionDef name params body))    
    [_, _] -> Left "Bitte benenne die Parameter der verschachtelten Funktionen verschieden"
    _ -> Left "ein anderer Fehler ist aufgetreten."
calculateExp (Application name arguments) table =
  case [(ident, def) | (ident, def) <- table, ident == name] of
    [] -> Left $ "Keine Funktion " ++ name ++ " vorhanden"
    [(_, FunctionDef _ params bodyExp)] -> calculateExp bodyExp $ table ++ paramsArgumentsMap params arguments
    _ -> Left $ name ++ " ist keine Funktion"

{-
    Nimmt den i-ten String als name und die i-te Expression, baut daraus eine Variablendefinition, und fügt diese in eine Symboltablle ein.
-}
paramsArgumentsMap :: [String] -> [Expression] -> SymbolTable
paramsArgumentsMap params arguments = map (\(param, arg) -> (param, VariableDef param arg)) (zip params arguments)

{-
    gibt zu einem Opcode die entsprechende Operation zurück
-}
operator :: OpCode -> Double -> Double -> Double
operator Add = (+)
operator Sub = (-)
operator Mul = (*)
operator Div = (/)
operator Pow = (**)


--------------------------------------------------------------------------------------------------------------------

-- BERECHNUNG NULLSTELLEN

buildApplication :: Definition -> Double -> Either String Expression
buildApplication (VariableDef _ _) _ = Left "Application Auf Variablendefinition"
buildApplication (FunctionDef name _ _) x = Right $ Application name [Number x]

{-
    setzt jeden Wert aus der Liste im Zweiten Argument in die Expression aus dem ersten Argument ein und gibt eine
    Liste aus Tupeln bestehend aus dem eingesetzten x-Wert und dem damit berechneten y-Wert zurück
-}
tryZeroPoints :: SymbolTable -> Definition -> [Double] -> Either String [(Double, Double)]
tryZeroPoints st def li =  case filterEither [(d, case buildApplication def d of 
    Left err -> Left err
    Right appDef ->  case calculateExp appDef st of 
        Left err -> Left err
        Right res -> Right res) 
    | d <- li] of 
        Left err -> Left err
        Right res -> Right res

filterEither :: [(Double, Either String Double)] -> Either String [(Double, Double)]
filterEither li = if not $ null [t | t <- li, isLeft $ snd t] then
    Left "Fehler bei Berechnung der Nullstellen" else
    Right [(fst t, case snd t of
        Right tRight -> tRight) | t <- li, isRight $ snd t]
{-
    filtert die Liste aus "tryZeroPoints" nach y-Werten, die sehr nahe an 0 sind
-}
findCloseToZero :: [(Double, Double)] -> [(Double, Double)]
findCloseToZero li = filter ((< 0.001) . snd) [(fst d, abs $ snd d) | d <- li]

{-
    nimmt eine Liste aus Tupeln entgegen und gibt eine Liste bestehend aus dem ersten Wert des Tupels mit 2 Nachkommastellen zurück.
-}
mapTupleToFirstString :: [(Double, Double)] -> [String]
mapTupleToFirstString tupLst = [printf "%.2f" $ fst d | d <- tupLst]

{-
    Funktion, die am Ende die Nullstellen berechnet
-}
calculateZeroPoints :: SymbolTable -> Definition -> Either String [String]
calculateZeroPoints st def = case tryZeroPoints st def [-10.0, -9.999 .. 10.0] of
    Right zp -> Right $ mapTupleToFirstString $ findCloseToZero zp
    Left err -> Left err
