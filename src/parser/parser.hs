
import Text.ParserCombinators.Parsec

zero = char '0'
dig = oneOf ['1' .. '9']
number = do
    c <- dig
    cs <- many (dig <|> zero)
    return (c : cs)
-- : [] = \x --> Lambda wird direkt an fmap übergeben (\x -> [x])
value = number <|> fmap (: []) (char 'x') 
opLow = char '+' <|> char '-'
opMed = char '*' <|> char '/'
function = 
    
    {-do
    f <- fmap function
    op <- fmap (: []) opLow
    rest <- exp1
    return (f : rest)-}
exp1 = do
    exp <- exp1
    op <- opMed
    rest <- exp2
    return (exp : rest) -- <|> exp2
exp2 = do
    minus <- char '-'
    rest <- exp3
    return (minus : rest) -- <|> exp3
exp3 = do
    lparen <- char '('
    f <- function
    rparen <- char ')'
    return (lparen : rparen)
    <|> value

parseFunction :: String -> Either ParseError String
parseFunction = parse function "(f)"