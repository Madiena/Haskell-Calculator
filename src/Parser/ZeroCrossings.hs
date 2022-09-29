module Parser.ZeroCrossings(replaceIdentifierInExpression, calculateZeroPoints, calculate, returnExpressionFromDef) where

import Parser.AbstractSyntax
import Text.Printf

--------------------------------------------------------------------------------------------------------------------

-- BERECHNUNG NULLSTELLEN

{-
    Nimmt eine Expression und eine Zahl entgegen und ersetzt jede Variable in der Expression mit dieser Zahl. ("Setzt die Zahl in die Funktion ein")
-}
replaceIdentifierInExpression :: Expression -> Double -> Expression
replaceIdentifierInExpression (Binary op opLeft opRight) x = Binary op (replaceIdentifierInExpression opLeft x) (replaceIdentifierInExpression opRight x)
replaceIdentifierInExpression (Number d) x = Number d
replaceIdentifierInExpression (Var _) x = Number x

{-
    setzt jeden Wert aus der Liste im Zweiten Argument in die Expression aus dem ersten Argument ein und gibt eine Liste aus Tupeln bestehend aus dem eingesetzten x-Wert 
    und dem damit berechneten y-Wert zurück
-}
tryZeroPoints :: Expression -> [Double] -> [(Double, Double)]
tryZeroPoints ex li = [(d, calculate (replaceIdentifierInExpression ex d)) | d <- li]

{-
    filtert die Liste aus "tryZeroPoints" nach y-Werten, die sehr nahe an 0 sind
-}

findCloseToZero :: [(Double, Double)] -> [(Double, Double)]
findCloseToZero li = filter ((< 0.001) . snd ) [(fst d, abs $ snd d) | d <- li]

{-
    nimmt eine Liste aus Tupeln entgegen und gibt eine Liste bestehend aus dem ersten Double zurück
-}

mapTupleToFirstString :: [(Double, Double)] -> [String]
mapTupleToFirstString tupLst = [printf "%.2f" $ fst d | d <- tupLst]


calculateZeroPoints :: Expression -> [String]
calculateZeroPoints ex = mapTupleToFirstString $ findCloseToZero $ tryZeroPoints ex [-10.0,-9.999..10.0 ]

returnExpressionFromDef :: Definition -> Expression
-- returnExpressionFromDef (def :: FunctionDef) = funcBody def
returnExpressionFromDef FunctionDef { funcBody = funcBody } = funcBody  
returnExpressionFromDef VariableDef { expr = expression} = expression


calculate :: Expression -> Double
calculate (Binary op opLeft opRight) = operator op (calculate opLeft) (calculate opRight)
calculate (Number d) = d     
calculate _ = undefined

operator :: OpCode -> Double -> Double -> Double
operator Add = (+)
operator Sub = (-)
operator Mul = (*)
operator Div = (/)
operator Pow = (**)



