module Service.Compiler where

import Control.Monad (join)
import Data.List (intercalate, intersperse)
import Service.AbstractSyntax
  ( Definition (FunctionDef, VariableDef),
    Expression (..),
    OpCode (Pow),
  )

--------------------------------------------------------------------------------------------------------------------

-- COMPILER

{-
    Kompiliert eine Definition (siehe abstrakter Systaxbaum) zu JavaScript Code
-}
compileToJS :: Definition -> String
compileToJS (FunctionDef name params body) = "function " ++ name ++ "(" ++ intercalate ", " params ++ ") {return " ++ compileExpToJS body ++ ";}"
compileToJS (VariableDef name body) = "variable " ++ name ++ " = " ++ compileExpToJS body

{-
    Kompiliert eine einzelne Expression (siehe absttrakter Syntaxbaum) zu JavaScript Code
-}
compileExpToJS :: Expression -> String
compileExpToJS (Var name) = name
compileExpToJS (Number i) = show i
compileExpToJS (Application name arguments) = name ++ "(" ++ argList ++ ")"
  where
    argList = join . intersperse "," $ fmap compileExpToJS arguments
compileExpToJS (Binary op exp1 exp2) =
  if op == Pow
    then "(" ++ "Math.pow(" ++ compileExpToJS exp1 ++ "," ++ compileExpToJS exp2 ++ "))"
    else "(" ++ compileExpToJS exp1 ++ show op ++ compileExpToJS exp2 ++ ")"
