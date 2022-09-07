
module Parser.Parser(function, parseFunction) where
import Text.ParserCombinators.Parsec hiding (token)
import Parser.AbstractSyntax
import Control.Monad


function :: Parser Expression
function = do
    name <- identifier
    tokenParser $ char '('
    param <- identifier
    tokenParser $ char ')'
    tokenParser $ char '='
    body <- simpleExprLow -- man holt Inhalt aus der Monade raus
    return $ Function name param body


identifier :: Parser String
-- [a-zA-Z][a-zA-Z0-9]*
-- join (fmap (\head -> fmap (\tail -> head : tail) secondPart ) firstPart) == firstPart >>= (\head -> fmap (\tail -> head : tail) secondPart)
identifier = do
    head <- firstPart
    tail <- secondPart
    return (head:tail)
    where
        firstPart :: Parser Char
        firstPart =  tokenParser $ oneOf (['a'..'z']++['A'..'Z'])
        secondPart :: Parser [Char]
        secondPart = many (tokenParser $ oneOf (['a'..'z']++['A'..'Z']++['0'..'9']))

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
    tokenParser $ char '-';
    Binary Sub (Number 0) <$> simpleExprHigh;
} <|> simpleExprHigh

-- () | value
simpleExprHigh :: Parser Expression
simpleExprHigh = number <|> var <|> do {
    tokenParser $ char '(';
    expr <- simpleExprLow;
    tokenParser $ char ')';
    return expr;
}


-- between spaces function reduzieren
number :: Parser Expression
number = do
    firstDigit <- head
    followingDigits <- tail
    return (Number $ read (firstDigit : followingDigits))
    where
        head :: Parser Char
        head = tokenParser $ oneOf ['1'..'9']
        tail :: Parser [Char]
        tail = many (tokenParser $ oneOf ['0'..'9'])

opParser :: OpCode -> Parser (Expression -> Expression -> Expression)
--fmap (const operator) (char c) = fmap (\_ -> Add) (char '+')
opParser operator = fmap (const  (Binary operator)) (tokenParser $ char (head.show$operator))

opCodeLow :: Parser (Expression -> Expression -> Expression)
opCodeLow = opParser Add <|> opParser Sub

opCodeMed :: Parser (Expression -> Expression -> Expression)
opCodeMed = opParser Mul <|> opParser Div

var :: Parser Expression
var = fmap Var identifier

application :: Parser Expression
application = undefined

tokenParser :: Parser Char -> Parser Char
tokenParser = between spaces spaces

parseFunction :: String -> Either ParseError Expression
parseFunction = parse (between spaces spaces function) "<undefined source>"

{-
-- inputToJavaScript "f(x)=x*x"  
-- ==> 
            "function (x: any) {
              return x * x;
            }"

-}

compileToJS :: Expression -> String 
compileToJS (Var name) = name 
compileToJS (Number i) = show i
compileToJS (Application name arguments) = name ++ "(" ++ argList ++ ")"
    where 
        argList = join . (intersperse "," ) $ (fmap compileToJS arguments)
compileToJS (Binary op exp1 exp2) = (compileToJS exp1) (show op) (compileToJS exp2)

inputToJavaScript :: String -> String 
inputToJavaScript inp= do
    funcExp <- parseFunction inp
    paramName <- funcExp Function param 
    "function (" ++ (param funcExp) ++ " : any) {\n\t return " ++ (compileToJS funcExp) ++ ";\n}"