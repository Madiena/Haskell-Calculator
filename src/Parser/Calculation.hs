{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Parser.Calculation(replaceIdentifierInExpression, calculate, returnExpressionFromDef) where

import Parser.AbstractSyntax
    ( Definition(VariableDef, FunctionDef, funcBody, expr),
      Expression(Number, Binary, Var),
      OpCode(Pow, Add, Sub, Mul, Div) )



returnExpressionFromDef :: Definition -> Expression
returnExpressionFromDef FunctionDef { funcBody = functionBody } = functionBody  
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
operator _ = undefined

{-
    Nimmt eine Expression und eine Zahl entgegen und ersetzt jede Variable in der Expression mit dieser Zahl. ("Setzt die Zahl in die Funktion ein")
-}
replaceIdentifierInExpression :: Expression -> Double -> Expression
replaceIdentifierInExpression (Binary op opLeft opRight) x = Binary op (replaceIdentifierInExpression opLeft x) (replaceIdentifierInExpression opRight x)
replaceIdentifierInExpression (Number d) _ = Number d
replaceIdentifierInExpression (Var _) x = Number x
replaceIdentifierInExpression _ _= undefined



