module Main where

import Data.List (intercalate)
import Parser.AbstractSyntax
  ( Definition (FunctionDef, VariableDef),
    Expression (..),
    OpCode (Add, Div, Mul, Pow, Sub),
  )
import Parser.REPL (repl)
import Parser.SymbolTable
  ( 
    Entry,
    SymbolTable,
    storeDefinition,
  )

main :: IO ()
main = do
  repl ([] :: SymbolTable)



