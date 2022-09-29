module Main where

import Repl
import Parser.SymbolTable

main :: IO ()
main = do
  repl ([] :: SymbolTable)



