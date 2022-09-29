{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Parser.Parser(functionDef, parseFunction, compileToJS, parseReplInput) where
import Text.ParserCombinators.Parsec hiding (token)
import Parser.AbstractSyntax
import Control.Monad
import Data.List 
import Parser.SymbolTable
--import Text.Read (Lexeme(String))

--chain :: Parser a -> Parser (a -> b -> b) -> Parser b

-- e.g.: f(x) = x * x
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

-- e.g.: h = 5
varDef :: Parser Definition
varDef = do
    name <- identifier
    token '='
    value <- simpleExprLow
    return $ VariableDef name value

identifier :: Parser String
-- [a-zA-Z][a-zA-Z0-9]*
-- join (fmap (\head -> fmap (\tail -> head : tail) secondPart ) firstPart) == firstPart >>= (\head -> fmap (\tail -> head : tail) secondPart)
identifier = do
    head <- firstPart
    tail <- secondPart
    return (head:tail)
    where
        --Möglichkeit ln als identifier zu nutzen muss verhindert sein

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
simpleExprMed = chainl1 simpleExprMediHigh opCodeMed

-- ^ (Power)
simpleExprMediHigh :: Parser Expression
simpleExprMediHigh = chainl1 simpleExprUnary opCodeMediHigh

-- negation 
simpleExprUnary :: Parser Expression
simpleExprUnary = do {
    token '-';
    Binary Sub (Number 0) <$> simpleExprHigh;
} <|> simpleExprHigh

-- () | value
simpleExprHigh :: Parser Expression
simpleExprHigh = try application <|> try var <|>  do {
    token '(';
    expr <- simpleExprLow;
    token ')';
    return expr;
}  <|> number


number :: Parser Expression
number = do
    -- x    0.0
    iP <- try integralPart <|> return "0"
    fP <- try fractionalPart <|> return "0"
    return (Number  (read (iP ++ ['.'] ++ fP) :: Double ) ) 
    where
        integralPart :: Parser String
        integralPart = integer
        fractionalPart :: Parser String  -- '.' 
        fractionalPart = (char '.' {-<|> char ',' -}) >> many1 ( oneOf ['0'..'9']) -- 1,2 ist nicht erlaubt, damit man später f(1,2) schriben kann für f(x,y)=x+y
   {-
    firstDigit <- head
    followingDigits <- tail
    return (Number $ read (firstDigit : followingDigits))
    where
        head :: Parser Char
        head = tokenParser $ oneOf ['0'..'9']
        tail :: Parser [Char]
        tail = many (tokenParser $ oneOf ['0'..'9'])
   
    
    do
    -}

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

opCodeMediHigh :: Parser (Expression -> Expression -> Expression)
opCodeMediHigh = opParser Pow

var :: Parser Expression
var = fmap Var identifier

-- f(Expression)   ; f(x-1)
application :: Parser Expression
application = do 
    functionName <- identifier
    token '('
    arguments <- sepBy1 simpleExprLow (char ',')
    token ')'
    return $ Application functionName arguments


tokenParser :: Parser Char -> Parser Char
tokenParser = between spaces spaces

token ::  Char -> Parser ()
token c = void (tokenParser $ char c)

parseFunction :: String -> Either ParseError Definition
parseFunction = parse (between spaces spaces (try functionDef <|> varDef)) "Problem beim Parser"

parseReplInput :: String -> Either String ReplInput
parseReplInput input = do 
    case parseFunction input of
        Right def -> Right (Def def) 
        Left error1 -> case parse simpleExprLow   "Problem beim Parsen von Expression"   input of 
            Right exp_parseReplInput -> Right $ Exp exp_parseReplInput
            Left error2 -> Left $ show error1 ++ " : " ++ show error2
        





{-
-- inputToJavaScript "f(x)=x*x"  
-- ==> 
            "function (x: any) {
              return x * x;
            }"

-}




compileToJS :: Definition -> String
compileToJS (FunctionDef name params body) = "function " ++ name ++ "(" ++ (intercalate ", ") params ++ ") {return " ++ compileExpToJS body ++ ";}"
compileToJS (VariableDef name body) = "variable " ++ name ++ " = " ++ compileExpToJS body

compileExpToJS :: Expression -> String
compileExpToJS (Var name) = name
compileExpToJS (Number i) = show i
compileExpToJS (Application name arguments) = name ++ "(" ++ argList ++ ")"
    where
    argList = join . intersperse "," $ fmap compileExpToJS arguments
compileExpToJS (Binary op exp1 exp2) = if op == Pow
    -- (Math.pow(2, 3)) == 2^3
    then "(" ++ "Math.pow(" ++ compileExpToJS exp1 ++ "," ++ compileExpToJS exp2 ++ "))"
    else "(" ++ compileExpToJS exp1 ++ show op ++ compileExpToJS exp2 ++ ")"



