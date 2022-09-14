module Parser.ZeroCrossings (calculateZeroPoints) where

import AbstractSyntax
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

