{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Parser.Parser(functionDef, parseFunction, compileToJS) where
import Text.ParserCombinators.Parsec hiding (token)
import Parser.AbstractSyntax
import Control.Monad
import Data.List 
import Text.Read (Lexeme(String))

--chain :: Parser a -> Parser (a -> b -> b) -> Parser b


functionDef :: Parser Definition
functionDef = do
    name <- identifier
    token '('
    params <- sepBy1 identifier (char ',') -- chainl1 (fmap return identifier) (char ',' >> return (++))     -- ToDo: merhrere Parameter erkennen können-- f(x) = x*x,    f(x,y)=x+y, 
    token ')'
    token '='
    body <- simpleExprLow -- man holt Inhalt aus der Monade raus
    return $ FunctionDef name params body

--parameterErkennung :: Parser [String] -> Parser [String]
{-
parameterErkennung oldParamList = do
    let neueParamListe <- (fmap (\ident ->  ident : oldParamList)   identifier)
    char ','  -- versuche ein ',' zu erkennen
    parameterErkennung neueParamListe
    --parameterErkennung  (fmap (\ident ->  ident : oldParamList)   identifier)
-}



    


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
    token '-';
    Binary Sub (Number 0) <$> simpleExprHigh;
} <|> simpleExprHigh

-- () | value
simpleExprHigh :: Parser Expression
simpleExprHigh = number <|>  try application <|> var <|>  do {
    token '(';
    expr <- simpleExprLow;
    token ')';
    return expr;
}  


-- between spaces function reduzieren
number :: Parser Expression
number = do -- 123.456
    iP <- try integralPart <|> return "0"
    fP <- try fractionalPart <|> return "0"
    return (Number $ read (iP ++ ['.'] ++ fP)) -- 5.0
    where 
        integralPart :: Parser String
        integralPart = integer
        fractionalPart :: Parser String  -- '.' 
        fractionalPart = (char '.' <|> char ',') >> many1 ( oneOf ['0'..'9'])
    

integer :: Parser String
integer = (many (tokenParser $ oneOf ['0'..'9']))

    {-
    do
    firstDigit <- head
    followingDigits <- tail
    return (Number $ read (firstDigit : followingDigits))
    where
        head :: Parser Char
        head = tokenParser $ oneOf ['0'..'9']
        tail :: Parser [Char]
        tail = many (tokenParser $ oneOf ['0'..'9'])
-}
opParser :: OpCode -> Parser (Expression -> Expression -> Expression)
--fmap (const operator) (char c) = fmap (\_ -> Add) (char '+')
opParser operator = fmap (const  (Binary operator)) (tokenParser $ char (head.show$operator))

opCodeLow :: Parser (Expression -> Expression -> Expression)
opCodeLow = opParser Add <|> opParser Sub

opCodeMed :: Parser (Expression -> Expression -> Expression)
opCodeMed = opParser Mul <|> opParser Div

var :: Parser Expression
var = fmap Var identifier

-- f(Expression)   ; f(x-1)
application :: Parser Expression
application = do 
    functionName <- identifier
    token '('
    argument <- simpleExprLow
    token ')'
    return $ Application functionName [argument]


tokenParser :: Parser Char -> Parser Char
tokenParser = between spaces spaces

token ::  Char -> Parser ()
token c = void (tokenParser $ char c)

parseFunction :: String -> Either ParseError Definition
parseFunction = parse (between spaces spaces functionDef) "Problem beim Parser"

{-
-- inputToJavaScript "f(x)=x*x"  
-- ==> 
            "function (x: any) {
              return x * x;
            }"

-}




compileToJS :: Definition -> String
compileToJS (FunctionDef name params body) = "function " ++ name ++ "(" ++ (intercalate ", ") params ++ ") {return " ++ compileExpToJS body ++ ";}"

compileExpToJS :: Expression -> String
compileExpToJS (Var name) = name
compileExpToJS (Number i) = show i
compileExpToJS (Application name arguments) = name ++ "(" ++ argList ++ ")"
    where
    argList = join . intersperse "," $ fmap compileExpToJS arguments
compileExpToJS (Binary op exp1 exp2) = "(" ++ compileExpToJS exp1 ++ show op ++ compileExpToJS exp2 ++ ")"



