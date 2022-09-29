{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use second" #-}
module Parser.ZeroCrossings(calculateZeroPoints) where

import Parser.AbstractSyntax ( Expression )
import Text.Printf ( printf )
import Pars

--------------------------------------------------------------------------------------------------------------------

-- BERECHNUNG NULLSTELLEN

{-
    setzt jeden Wert aus der Liste im Zweiten Argument in die Expression aus dem ersten Argument ein und gibt eine 
    Liste aus Tupeln bestehend aus dem eingesetzten x-Wert und dem damit berechneten y-Wert zurück
-}
tryZeroPoints :: Expression -> [Double] -> [(Double, Double)]
tryZeroPoints ex li = [(d, calculate (replaceIdentifierInExpression ex d)) | d <- li]

{-
    filtert die Liste aus "tryZeroPoints" nach y-Werten, die sehr nahe an 0 sind
-}

findCloseToZero :: [(Double, Double)] -> [(Double, Double)]
findCloseToZero li = filter ((< 0.001) . snd ) [(fst d, abs $ snd d) | d <- li]

{-
    nimmt eine Liste aus Tupeln entgegen und gibt eine Liste bestehend aus dem ersten Double zurück. Die ersten 2 
    Nachkomma
-}
mapTupleToFirstString :: [(Double, Double)] -> [String]
mapTupleToFirstString tupLst = [printf "%.2f" $ fst d | d <- tupLst]

{-
    Funktion, die am Ende die Nullstellen berechnet
-}
calculateZeroPoints :: Expression -> [String]
calculateZeroPoints ex = mapTupleToFirstString $ findCloseToZero $ tryZeroPoints ex [-10.0,-9.999..10.0 ]
