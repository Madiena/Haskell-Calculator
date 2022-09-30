module Main where

import Service.REPL (repl)
import Service.SymbolTable (SymbolTable)

--------------------------------------------------------------------------------------------------------------------

-- MAIN

main :: IO ()
main = do
  repl ([] :: SymbolTable)
