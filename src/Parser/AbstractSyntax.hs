module Parser.AbstractSyntax(Expression(..), OpCode(..), Definition(..), calculate, returnExpressionFromDef) where
import Data.Functor.Contravariant (Op)

{-
data Expr argumentType = 
      Var String 
    | Application String [argumentType]
    deriving (Show, Eq)

type LExpr = Expr LExpr 
data RExpr = RExpr (Expr RExpr) | Number Integer | Binary OpCode RExpression RExpression
-}

--name (Var n) = n
-- type MyInt = Int <- findet nur auf der Typebene statt
-- data definiert einen neuen Typen

--data Definition = Definition LExpression RExpression -- f(x) = 4; x = 5;  in Version 2: a[i] = 8;
data Definition = FunctionDef {funcName :: String , funcParams :: [String], funcBody :: Expression} 
                | VariableDef String Expression

--data LExpression = LVar String | LApplication {name :: String, param :: [String]}

data Expression = -- Function {name :: String, param :: String, expr :: Expression} 
      Binary OpCode Expression Expression -- functionApplication 
    | Var String 
    | Application String [Expression]
    | Number Double
    deriving (Show, Eq)

returnExpressionFromDef :: Definition -> Expression
-- returnExpressionFromDef (def :: FunctionDef) = funcBody def
returnExpressionFromDef (FunctionDef { funcBody = funcBody }) = funcBody  

calculate :: Expression -> Double
calculate (Binary op opLeft opRight) = (operator op) (calculate opLeft) (calculate opRight)
calculate (Number d) = d
calculate _ = undefined



operator :: OpCode -> Double -> Double -> Double
operator Add = (+)
operator Sub = (-)
operator Mul = (*)
operator Div = (/)

{-
data RExpression = -- Function {name :: String, param :: String, expr :: Expression} 
      Binary OpCode RExpression RExpression -- functionApplication 
    | RVar String 
    | RApplication String [RExpression]
    | Number Integer
    deriving (Show, Eq)
-}

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
