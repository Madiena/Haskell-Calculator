
module Parser.Parser(function, parseFunction) where
import Text.ParserCombinators.Parsec hiding (token)
import Parser.AbstractSyntax
import Control.Monad


function :: Parser Expression
function = do
    name <- identifier
    token '('
    param <- identifier
    token ')'
    token '='
    simpleExprLow

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

--Binary Expression wird zur Simple Expression durch Konzept
--der Präzedenzen

-- + & -
simpleExprLow :: Parser Expression
simpleExprLow = chainl1 simpleExprMed opCodeLow

-- * & /
simpleExprMed :: Parser Expression
simpleExprMed = chainl1 simpleExprUnary opCodeMed

-- negation
simpleExprUnary :: Parser Expression
simpleExprUnary = do {
    token '-';
    Binary Sub (Number 0) <$> simpleExprHigh;
} <|> simpleExprHigh

-- () | value
simpleExprHigh :: Parser Expression
simpleExprHigh = number <|> var <|> do {
    token '(';
    expr <- simpleExprLow;
    token ')';
    return expr;
}

number :: Parser Expression
number = do
    firstDigit <- head
    followingDigits <- tail
    return (Number $ read (firstDigit : followingDigits))
    where
        head :: Parser Char
        head = oneOf ['1'..'9']
        tail :: Parser [Char]
        tail = many (oneOf ['0'..'9'])

opParser :: OpCode -> Parser (Expression -> Expression -> Expression)
--fmap (const operator) (char c) = fmap (\_ -> Add) (char '+')
opParser operator = fmap (const  (Binary operator)) (token (head.show$operator))

opCodeLow :: Parser (Expression -> Expression -> Expression)
opCodeLow = opParser Add <|> opParser Sub

opCodeMed :: Parser (Expression -> Expression -> Expression)
opCodeMed = opParser Mul <|> opParser Div

var :: Parser Expression
var = fmap Var identifier

application :: Parser Expression
application = undefined

token :: Char -> Parser Char
token c = between spaces spaces $ char c

parseFunction :: String -> Either ParseError Expression
parseFunction = parse (between spaces spaces function) "<undefined source>"