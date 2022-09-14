module Parser.ZeroCrossings(replaceIdentifierInExpression, calculateZeroPoints, calculate, returnExpressionFromDef) where

import Parser.AbstractSyntax
import Data.List (sortOn)

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

