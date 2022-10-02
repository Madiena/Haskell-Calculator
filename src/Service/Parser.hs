{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Service.Parser (functionDef, parseDefinition, simpleExprLow, returnExpressionFromDef) where

import Control.Monad (void)
import Service.AbstractSyntax
  ( Definition (FunctionDef, VariableDef, expr, funcBody),
    Expression (..),
    OpCode (..),
  )
import Text.ParserCombinators.Parsec
  ( ParseError,
    Parser,
    between,
    chainl1,
    char,
    many,
    many1,
    oneOf,
    parse,
    sepBy1,
    spaces,
    try,
    (<|>),
  )

--------------------------------------------------------------------------------------------------------------------

-- AUFRUFEN DES PARSERS

parseDefinition :: String -> Either ParseError Definition
parseDefinition = parse (between spaces spaces (try functionDef <|> varDef)) "Problem beim Parser"

--------------------------------------------------------------------------------------------------------------------

-- PARSER FÜR GANZE DEFINITIONEN

{-
    setzt die Bestandteile einer Funktionsdefinition zu einer Definition zusammen
-}
functionDef :: Parser Definition
functionDef = do
  name <- identifier
  token '('
  params <- sepBy1 identifier (char ',')
  token ')'
  token '='
  body <- simpleExprLow
  return $ FunctionDef name params body

{-
    setzt die Bestandteile einer Variablendefinition zu einer Definition zusammen
-}
varDef :: Parser Definition
varDef = do
  name <- identifier
  token '='
  value <- simpleExprLow
  return $ VariableDef name value

--------------------------------------------------------------------------------------------------------------------

{-
    Parst einen einzelnen Identifier beginnend mit einem großen oder kleinen Buchstaben und danach beliebig viele Buchstaben
    oder Zahlen
-}
identifier :: Parser String
identifier = do
  head <- firstPart
  tail <- secondPart
  return (head : tail)
  where
    firstPart :: Parser Char
    firstPart = tokenParser $ oneOf (['a' .. 'z'] ++ ['A' .. 'Z'])
    secondPart :: Parser [Char]
    secondPart = many (tokenParser $ oneOf (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']))

{-
    parst einen Integer
-}
integer :: Parser String
integer = many (tokenParser $ oneOf ['0' .. '9'])

{-
    parst eine einzelne Zahl mit Nachkommastellen
-}
number :: Parser Expression
number = do
  iP <- try integralPart <|> return "0"
  fP <- try fractionalPart <|> return "0"
  return (Number (read (iP ++ ['.'] ++ fP) :: Double))
  where
    integralPart :: Parser String
    integralPart = integer
    fractionalPart :: Parser String
    fractionalPart = char '.' >> many1 (oneOf ['0' .. '9'])

{-
    parst Variablen mit beliebigem gültigen Bezeichner
-}
var :: Parser Expression
var = fmap Var identifier

--------------------------------------------------------------------------------------------------------------------

-- EXPRESSIONS

{-
    parst eine Expression bestehend aus einer Addition oder Subtraktion

    + & -
-}
simpleExprLow :: Parser Expression
simpleExprLow = chainl1 simpleExprMed opCodeLow

{-
    parst eine Expression bestehend aus einer Multiplikation oder Division

    * & /
-}
simpleExprMed :: Parser Expression
simpleExprMed = chainl1 simpleExprMediHigh opCodeMed

{-
    parst eine Expression mit einer Potenz

    ^ (Power)

-}
simpleExprMediHigh :: Parser Expression
simpleExprMediHigh = chainl1 simpleExprUnary opCodeMediHigh

{-
    parst eine Expression bestehend aus einer Negation
-}
simpleExprUnary :: Parser Expression
simpleExprUnary =
  do
    token '-'
    Binary Sub (Number 0) <$> simpleExprHigh
    <|> simpleExprHigh

{-
    parst eine geklammerte Expression oder einen Wert, der
    aus einer Funktionsanwendung, einer Variable oder eine Zahl
    resultieren kann
-}
simpleExprHigh :: Parser Expression
simpleExprHigh =
  try application <|> try var <|> do
    token '('
    expr <- simpleExprLow
    token ')'
    return expr
    <|> number

{-
    Hilfsfunktion, die aus einem beliebigen Operator eine weiter-
    verwendbare Binary Expression erstellt
-}
opParser :: OpCode -> Parser (Expression -> Expression -> Expression)
--fmap (const operator) (char c) = fmap (\_ -> Add) (char '+')
opParser operator = fmap (const (Binary operator)) (tokenParser $ char (head . show $ operator))

{-
    erstellt eine Binary Expression mit niedrigster Präzedenz
    aus den Operatoren + oder -
-}
opCodeLow :: Parser (Expression -> Expression -> Expression)
opCodeLow = opParser Add <|> opParser Sub

{-
    erstellt eine Binary Expression mit mittlerer Präzedenz
    aus den Operatoren * oder /
-}
opCodeMed :: Parser (Expression -> Expression -> Expression)
opCodeMed = opParser Mul <|> opParser Div

{-
    erstellt eine Binary Expression mit bislang höchster Präzedenz
    aus dem Potenz Operator
-}
opCodeMediHigh :: Parser (Expression -> Expression -> Expression)
opCodeMediHigh = opParser Pow

{-
    parst eine Funktionsanwendung in der Form f(x)
-}
application :: Parser Expression
application = do
  functionName <- identifier
  token '('
  arguments <- sepBy1 simpleExprLow (char ',')
  token ')'
  return $ Application functionName arguments

--------------------------------------------------------------------------------------------------------------------

-- HILFSFUNKTIONEN ÜBER ALLE KATEGORIEN HINWEG

{-
    Hilfsfunktion, die erlaubt beliebig viele Whitespaces
    zu parsen, ohne ihnen eine semantische Bedeutung zuzordnen,
    oder sie als Parser Fehler zu interpretieren
-}
tokenParser :: Parser Char -> Parser Char
tokenParser = between spaces spaces

{-
    Hilfsfunktion, die das problemlose Parsen von Whitespaces
    vor und nach einem gelesenen Zeichen ermöglicht
-}
token :: Char -> Parser ()
token c = void (tokenParser $ char c)

returnExpressionFromDef :: Definition -> Expression
returnExpressionFromDef FunctionDef {funcBody = functionBody} = functionBody
returnExpressionFromDef VariableDef {expr = expression} = expression