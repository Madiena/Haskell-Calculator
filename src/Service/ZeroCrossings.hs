{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use second" #-}

module Service.ZeroCrossings (calculateZeroPoints) where

import Service.AbstractSyntax (Expression (Application, Number), Definition (VariableDef, FunctionDef))
import Service.Calculation (calculateExp)
import Text.Printf (printf)
import Service.SymbolTable (SymbolTable)

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
tryZeroPoints st def li = [(d, calculateExp (buildApplication def d) st) | d <- li]

{-
    filtert die Liste aus "tryZeroPoints" nach y-Werten, die sehr nahe an 0 sind
-}
findCloseToZero :: [(Double, Double)] -> [(Double, Double)]
findCloseToZero li = filter ((< 0.001) . snd) [(fst d, abs $ snd d) | d <- li]

{-
    nimmt eine Liste aus Tupeln entgegen und gibt eine Liste bestehend aus dem ersten Wert des Tupels mit 2 Nackommastellen zurück.
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
