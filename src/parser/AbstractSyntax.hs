module AbstractSyntax(Expression(..)) where

data Expression = Function String Expression 
    | Binary Expression OpCode Expression 
    | Var  String 
    | Application String [Expression]
    deriving (Show, Eq)


data OpCode = Add | Sub | Mul | Div deriving (Show, Eq)

{-instance Show a => Show (Expression a) where
show (Function ident expr) = "Function(" ++ Prelude.show ident ++ ") = (" ++ AbstractSyntax.show expr ++ ")"
show (Op left opCode right) = "Left Expression(" ++ AbstractSyntax.show left ++ "), Operator(" ++ Prelude.show opCode ++ "), Right Expression(" ++ AbstractSyntax.show right ++ ")" 
show (Var ident) = "Variable(" ++ Prelude.show ident ++ ")"
show (Application ident expr) = Prelude.show ident ++ "(x) = " + AbstractSyntax.show expr-}
