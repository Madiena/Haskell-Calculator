module AbstractSyntax(Expression) where

data Expression = Function String Expression 
    | Op Expression OpCode Expression 
    | Var  String 
    | Application String [Expression]
    deriving Show


data OpCode = Add | Sub | Mul | Div deriving Show
