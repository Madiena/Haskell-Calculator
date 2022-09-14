module Parser.ZeroCrossings where

import AbstractSyntax

replaceIdentifierInExpression :: Expression -> Double -> Expression
replaceIdentifierInExpression (Binary op opLeft opRight) x = Binary op (replaceIdentifierInExpression opLeft x) (replaceIdentifierInExpression opRight x)
replaceIdentifierInExpression (Number d) x = Number d
replaceIdentifierInExpression (Var name) x = Number x

--tryZeroPoints :: Expression -> [Double] -> [Double]
--tryZeroPoints :: 