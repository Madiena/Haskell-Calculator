
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
    simpleExpr

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

number :: Parser Expression
number = do
    firstDigit <- head
    followingDigits <- tail
    return (Number (firstDigit : followingDigits))
    where
        head :: Parser Char
        head = oneOf ['1'..'9']
        tail :: Parser [Char]
        tail = many (oneOf ['0'..'9'])

binary :: Parser Expression
binary = do
    simpleExpr
    opCode
    simpleExpr

opCode :: Parser Char
opCode = char '+' <|> char '-' <|> char '*' <|> char '/'

var :: Parser Expression
var = fmap Var identifier

application :: Parser Expression
application = undefined

parseFunction :: String -> Either ParseError Expression
parseFunction = parse function "<undefined source>"