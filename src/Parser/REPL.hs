module Parser.REPL (repl) where

import Parser.AbstractSyntax ( Definition, Expression )
import Parser.SymbolTable ( SymbolTable, storeDefinition, updateTable)
import Parser.Calculation ( calculateExp )
import Parser.Parser (parseReplInput)

{-
    Ein REPL Input besteht entweder aus einer Definition oder einer Expression
-}
data ReplInput = Def Definition | Exp Expression

{-
    Show Instanz für einen REPL Input
-}
instance Show ReplInput where 
    show (Def d)  = show d 
    show (Exp exp1) = show exp1

repl :: SymbolTable -> IO ()
repl table = do
  input <- getLine
  case parseReplInput input of
    Left err -> print err >> repl table
    Right entry ->
      case entry of
        Def definition -> do
          case updateTable (storeDefinition definition) table of
            Right newTable -> repl newTable
            Left err -> print err >> repl table
        Exp expression -> do
          case calculateExp expression table of
            Right erg -> print (show expression ++ " => " ++ show erg) >> repl table
            Left err -> print err >> repl table
