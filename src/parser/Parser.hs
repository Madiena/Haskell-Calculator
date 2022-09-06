
module Parser(function, parseFunction) where
import Text.ParserCombinators.Parsec
import AbstractSyntax
import Control.Monad
import Text.Read (Lexeme(Char))

function :: Parser Expression
function = do 
    name <- identifier
    char '('
    param <- identifier
    char ')'
    char '='

    fmap Var identifier

identifier :: Parser String
-- [a-zA-Z][a-zA-Z0-9]*
-- join (fmap (\head -> fmap (\tail -> head : tail) secondPart ) firstPart) == firstPart >>= (\head -> fmap (\tail -> head : tail) secondPart)
identifier = do
    head <- firstPart
    tail <- secondPart
    return (head:tail) 
    where
        firstPart :: Parser Char
        firstPart = oneOf (['a'..'z']++['A'..'Z'])
        secondPart :: Parser [Char]
        secondPart = many (oneOf (['a'..'z']++['A'..'Z']++['0'..'9']))

simpleExpr :: Parser Expression
simpleExpr = binary <|> var <|> application

zero = char '0'
dig = oneOf ['1'..'9']

number :: Parser Expression
number = do
    head :: Parser Char
    

binary :: Parser Expression
binary = undefined

var :: Parser Expression
var = undefined

application :: Parser Expression
application = undefined

parseFunction :: String -> Either ParseError Expression
parseFunction = parse function "<undefined source>"

{-function = chainl1 exp1 opLow
 do
    f <- function
    op <- opLow
    rest <- exp1
    return (f : rest)-}

{-exp1 = do
    exp <- exp1
    op <- opMed
    rest <- exp2
    return (exp : rest) <|> exp2-}

{-exp2 = do
    minus <- char '-'
    rest <- exp3
    return (minus : rest) <|> exp3-}

{-exp3 = do
    lparen <- char '('
    f <- function
    rparen <- char ')'
    return (lparen : rparen)
    <|> value-}

{-opLow = char '+' <|> char '-'

opMed = char '*' <|> char '/'
-}

{-}
number = do
    c <- dig
    cs <- many (dig <|> zero)
    return (c : cs)
-- : [] = \x --> Lambda wird direkt an fmap übergeben (\x -> [x])
value = number <|> fmap (: []) (char 'x') -}

