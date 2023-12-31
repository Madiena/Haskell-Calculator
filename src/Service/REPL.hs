module Service.REPL (repl, parseReplInput) where

import Service.AbstractSyntax (Definition(VariableDef, FunctionDef), Expression)
import Service.Calculation (calculateExp)
import Service.Parser (parseDefinition, simpleExprLow)
import Service.SymbolTable (SymbolTable, storeDefinition, updateTable)
import Text.ParserCombinators.Parsec (parse)
import Service.ZeroCrossings(calculateZeroPoints)

--------------------------------------------------------------------------------------------------------------------

-- REPL

{-
    Ein REPL Input besteht entweder aus einer Definition oder einer Expression
-}
data ReplInput = Def Definition | Exp Expression

{-
    Show Instanz für einen REPL Input
-}
instance Show ReplInput where
  show (Def d) = show d
  show (Exp exp1) = show exp1

{-
    Parst eine Funktionsdefinition oder Variablendefinition um sie als Eingabe an die REPL weiterzureichen,
    sofern die Eingabe valide ist.
    Ist die Eingabe invalide, wird eine Fehlernachricht ausgegeben.
-}
parseReplInput :: String -> Either String ReplInput
parseReplInput input= do
  case parseDefinition input of
    Right def -> Right (Def def)
    Left error1 -> case parse simpleExprLow "Problem beim Parsen von Expression" input of
      Right exp_parseReplInput -> Right $ Exp exp_parseReplInput
      Left error2 -> Left $ show error1 ++ " : " ++ show error2

{-
    REPL in einer Endlos-Schleife, die sämtliche Funktionalität des Rechners abbildet (mit Ausnahme der Grafikausgabe)
-}
repl :: SymbolTable -> IO ()
repl table = do
  input <- getLine
  case parseReplInput input of
    Left err -> print err >> repl table
    Right entry ->
      case entry of
        Def definition -> do
          case updateTable (storeDefinition definition) table of
            Right newTable -> case definition of 
              FunctionDef {} -> case calculateZeroPoints newTable definition of
                Right zp -> print ("zeropoints: " ++ show zp) >> repl newTable 
                Left err -> repl newTable
              VariableDef {} -> repl newTable
            Left err -> print err >> repl table
        Exp expression -> do
          case calculateExp expression table of
            Right erg -> print (show expression ++ " => " ++ show erg) >> repl table
            Left err -> print err >> repl table
