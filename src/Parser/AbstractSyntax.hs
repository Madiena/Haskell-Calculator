module Parser.AbstractSyntax(Expression(..), OpCode(..), Definition(..)) where
import Data.Functor.Contravariant (Op)
--import Parser.Parser
import Data.List 
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
                | VariableDef {varName :: String, expr :: Expression} deriving (Show, Eq)

{-
instance Show Definition where 
    show (FunctionDef name params body) = name ++ "(" ++ intercalate ", " (show `map` params) ++ ")=" ++ show body
    show (VariableDef name expr) = name ++ "=" ++ show expr
-}
--data LExpression = LVar String | LApplication {name :: String, param :: [String]}

data Expression = -- Function {name :: String, param :: String, expr :: Expression} 
      Binary OpCode Expression Expression -- functionApplication 
    | Var String 
    | Application String [Expression]
    | Number Double
    deriving (Eq)

instance Show Expression where 
    show (Binary o e1 e2)   = show e1 ++ show o ++ show e2 
    show (Var s) = s 
    show (Application name args) = name ++ "(" ++ intercalate ", " (show `map` args) ++ ")"
    show (Number d) = show d 
  




{-
data RExpression = -- Function {name :: String, param :: String, expr :: Expression} 
      Binary OpCode RExpression RExpression -- functionApplication 
    | RVar String 
    | RApplication String [RExpression]
    | Number Integer
    deriving (Show, Eq)
-}

--Konstruktor für data wie anwenden?
data OpCode = Add | Sub | Mul | Div | Pow | Log deriving (Eq)

instance Show OpCode where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show Pow = "^"
    show Log = "ln"

{-instance Show a => Show (Expression a) where
show (Function ident expr) = "Function(" ++ Prelude.show ident ++ ") = (" ++ AbstractSyntax.show expr ++ ")"
show (Op left opCode right) = "Left Expression(" ++ AbstractSyntax.show left ++ "), Operator(" ++ Prelude.show opCode ++ "), Right Expression(" ++ AbstractSyntax.show right ++ ")" 
show (Var ident) = "Variable(" ++ Prelude.show ident ++ ")"
show (Application ident expr) = Prelude.show ident ++ "(x) = " + AbstractSyntax.show expr-}
