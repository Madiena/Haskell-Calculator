module Parser.ZeroCrossings(replaceIdentifierInExpression, calculateZeroPoints, calculate, returnExpressionFromDef) where

import Parser.AbstractSyntax
import Data.List (sortOn)
import Text.ParserCombinators.Parsec

replaceIdentifierInExpression :: Expression -> Double -> Expression
replaceIdentifierInExpression (Binary op opLeft opRight) x = Binary op (replaceIdentifierInExpression opLeft x) (replaceIdentifierInExpression opRight x)
replaceIdentifierInExpression (Number d) x = Number d
replaceIdentifierInExpression (Var name) x = Number x

tryZeroPoints :: Expression -> [Double] -> [(Double, Double)]
tryZeroPoints ex li = [(d, calculate (replaceIdentifierInExpression ex d)) | d <- li]

findClosest :: [(Double, Double)] -> Double
findClosest li = fst $ head $ sortOn snd [(fst d, abs (snd d)) | d <- li]

calculateZeroPoints :: Expression -> Double
calculateZeroPoints ex = findClosest $ tryZeroPoints ex [-10.0,9.991..10.0 ]

returnExpressionFromDef :: Definition -> Expression
-- returnExpressionFromDef (def :: FunctionDef) = funcBody def
returnExpressionFromDef (FunctionDef { funcBody = funcBody }) = funcBody  

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
calculate (Binary op opLeft opRight) = (operator op (calculate opLeft) (calculate opRight))
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

