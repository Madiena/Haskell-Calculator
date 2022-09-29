module Parser.AbstractSyntax(Expression(..), OpCode(..), Definition(..)) where

import Data.List ( intercalate ) 

--------------------------------------------------------------------------------------------------------------------

-- AUFBAU DES ABSTRAKTEN SYNTAXBAUM

{-
    Eine Definition kann entweder eine Funktionsdefinition in der Form f(x)=Expression oder eine Variablendefinition
    in der Form Identifier = Wert sein
-}
data Definition = FunctionDef { funcName :: String , funcParams :: [String], funcBody :: Expression } 
                | VariableDef { varName :: String, expr :: Expression } deriving (Show, Eq)


{-
    Eine Expression kann entweder eine Binary Expression sein, eine Variable, eine Funktionsanwendung oder eine Zahl
-}

data Expression = 
      Binary OpCode Expression Expression
    | Var String 
    | Application String [Expression]
    | Number Double
    deriving (Eq)

{-
    Operatoren, die unterstützt werden
-}
data OpCode = Add | Sub | Mul | Div | Pow | Log deriving (Eq)

--------------------------------------------------------------------------------------------------------------------

-- SHOW INSTANZEN FÜR DEN ABSTRAKTEN SYNTAXBAUM

instance Show Expression where 
    show (Binary o e1 e2)   = show e1 ++ show o ++ show e2 
    show (Var s) = s 
    show (Application name args) = name ++ "(" ++ intercalate ", " (show `map` args) ++ ")"
    show (Number d) = show d 

instance Show OpCode where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show Pow = "^"
    show Log = "ln"
