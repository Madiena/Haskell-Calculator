module Parser.REPL where

import Parser.AbstractSyntax ( Definition, Expression )

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