module Handler.Helpers (mapToEntry, parseDefs, filterRights) where

import Service.AbstractSyntax
import Text.Parsec (ParseError)
import Service.SymbolTable
import Data.Either
import Service.Parser

--------------------------------------------------------------------------------------------------------------------
-- EINIGE HELFERFUNKTIONEN FÜR DIE HANDLER


{-
    trägt ein Array aus Definitionen in eine Symboltabelle ein
-}
mapToEntry :: [Definition] -> SymbolTable
mapToEntry li =
  [ storeDefinition d | d <- li ]


{-
    parst ein Array aus Strings
-}
parseDefs :: [String] -> Either String [Definition]
parseDefs li = case filterRights [parseDefinition d | d <- li] of
  Left err -> Left err
  Right defs -> Right defs


{-
    nimmt ein Array entgegen, in dem entweder eine Definition oder eine Fehlermeldung enthalten ist und gibt nur
    die Einträge zurück, bei denen es sich nicht um Fehler handelt
-}
filterRights :: [Either ParseError Definition] -> Either String [Definition]
filterRights li = if not $ null [t | t <- li, isLeft t] then
    Left "Fehler bei Berechnung" else
    Right [case t of
      Right tRight -> tRight 
    | t <- li, isRight t] 