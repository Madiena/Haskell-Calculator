module Parser.AbstractSyntax(Expression(..), OpCode(..)) where
import Data.Functor.Contravariant (Op)

data Expression = Function {name :: String, param :: String, expr :: Expression} 
    | Binary OpCode Expression Expression
    | Var  String 
    | Application String [Expression]
    | Number Integer
    deriving (Show, Eq)

--Konstruktor für data wie anwenden?
data OpCode = Add | Sub | Mul | Div deriving (Eq)

instance Show OpCode where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"

{-instance Show a => Show (Expression a) where
show (Function ident expr) = "Function(" ++ Prelude.show ident ++ ") = (" ++ AbstractSyntax.show expr ++ ")"
show (Op left opCode right) = "Left Expression(" ++ AbstractSyntax.show left ++ "), Operator(" ++ Prelude.show opCode ++ "), Right Expression(" ++ AbstractSyntax.show right ++ ")" 
show (Var ident) = "Variable(" ++ Prelude.show ident ++ ")"
show (Application ident expr) = Prelude.show ident ++ "(x) = " + AbstractSyntax.show expr-}
