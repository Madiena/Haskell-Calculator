module Parser.Compiler where

import Parser.AbstractSyntax
    ( Definition(VariableDef, FunctionDef),
      Expression(..),
      OpCode(Pow) )
import Data.List ( intercalate, intersperse )
import Control.Monad ( join )

compileToJS :: Definition -> String
compileToJS (FunctionDef name params body) = "function " ++ name ++ "(" ++ intercalate ", " params ++ ") {return " ++ compileExpToJS body ++ ";}"
compileToJS (VariableDef name body) = "variable " ++ name ++ " = " ++ compileExpToJS body

compileExpToJS :: Expression -> String
compileExpToJS (Var name) = name
compileExpToJS (Number i) = show i
compileExpToJS (Application name arguments) = name ++ "(" ++ argList ++ ")"
    where
    argList = join . intersperse "," $ fmap compileExpToJS arguments
compileExpToJS (Binary op exp1 exp2) = if op == Pow
    then "(" ++ "Math.pow(" ++ compileExpToJS exp1 ++ "," ++ compileExpToJS exp2 ++ "))"
    else "(" ++ compileExpToJS exp1 ++ show op ++ compileExpToJS exp2 ++ ")"


