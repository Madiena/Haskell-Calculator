module Parser.ZeroCrossings(replaceIdentifierInExpression, calculateZeroPoints, calculate, returnExpressionFromDef) where

import Parser.AbstractSyntax
import Text.Printf

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
returnExpressionFromDef VariableDef { variable = expr} = expr 
-- Unterscheidung zwischen Division durch 0 und anderen Operationen funktioniert!
-- Möglichkeit ParseError oder Ähnliches abzubilden fehlt -
-- benötigt um MathError bei der Division durch 0 darzustellen
-- Folgender Versuch funktioniert nicht:

{- calculate :: Expression -> Either ParseError Double
calculate (Binary op opLeft opRight) = do
    if op == Div && opRight == Number 0 
     then Left err <- "Math Error: Division by Zero is not allowed" :: ParseError
     else Right res <- (operator op (calculate opLeft) (calculate opRight))
calculate (Number d) = d 
--calculate (Var v) = -- replaceIdentifierInExpression v  --  f(x) = x*x + 2*x   5      
calculate _ = undefined
-}

-- Möglichkeit Division durch 0 ohne sinnvollen Output zu testen.
-- Ergebnis hierbei immer 2.0

{- calculate :: Expression -> Double
calculate (Binary op opLeft opRight) = do
    if op == Div && opRight == Number 0 
     then 1 + 1
     else (operator op (calculate opLeft) (calculate opRight))
calculate (Number d) = d 
--calculate (Var v) = -- replaceIdentifierInExpression v  --  f(x) = x*x + 2*x   5      
calculate _ = undefined
-}

calculate :: Expression -> Double
calculate (Binary op opLeft opRight) = operator op (calculate opLeft) (calculate opRight)
calculate (Number d) = d 
--calculate (Var v) = -- replaceIdentifierInExpression v  --  f(x) = x*x + 2*x   5      
calculate _ = undefined

operator :: OpCode -> Double -> Double -> Double
operator Add = (+)
operator Sub = (-)
operator Mul = (*)
operator Div = (/)
operator Pow = (**)

--division :: OpCode -> Double -> Double -> Double
--division Div = (/)

