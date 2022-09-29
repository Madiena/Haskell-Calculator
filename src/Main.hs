module Main where

import Data.List (intercalate)
import Parser.AbstractSyntax
  ( Definition (FunctionDef, VariableDef),
    Expression (..),
    OpCode (Add, Div, Mul, Pow, Sub),
  )
import Parser.Parser (parseReplInput)
import Parser.SymbolTable
  ( 
    Entry,
    SymbolTable,
    storeDefinition,
  )
import Parser.REPL ( ReplInput(Exp, Def) )

main :: IO ()
main = do
  repl ([] :: SymbolTable)



