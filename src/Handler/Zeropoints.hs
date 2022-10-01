{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Zeropoints where

import Data.Either
import Data.Aeson ()
import Foundation (Handler)
import Handler.JSONTypes
  ( CalcIn (funString, vars),
    Result (Result),
  )
import Import.NoFoundation
  ( Value,
    requireInsecureJsonBody,
    returnJson,
  )
import Service.AbstractSyntax (Definition)
import Service.Parser (parseDefinition)
import Service.SymbolTable (SymbolTable, storeDefinition, updateTable)
import Service.ZeroCrossings
  ( calculateZeroPoints,
  )
import Text.Parsec (ParseError)
import Yesod.Core.Content ()

--------------------------------------------------------------------------------------------------------------------

{-
    POST Request, der ein JSON mit dem eingegebenen String entgegen nimmt und die Nullstellen und die geparste
    Funktion also JSON zurückgibt
-}
mapToEntry :: [Definition] -> SymbolTable
mapToEntry li =
  [ storeDefinition d | d <- li ]

parseDefs :: [String] -> Either String [Definition]
parseDefs li = case filterRights [parseDefinition d | d <- li] of
  Left err -> Left err
  Right defs -> Right defs

filterRights :: [Either ParseError Definition] -> Either String [Definition]
filterRights li = if not $ null [t | t <- li, isLeft t] then
    Left "Fehler bei Berechnung" else
    Right [case t of
      Right tRight -> tRight 
    | t <- li, isRight t]

postZeroR :: Handler Value
postZeroR = do
  fun <- requireInsecureJsonBody :: Handler CalcIn
  case parseDefinition $ funString fun of
    Right parsedFun -> case parseDefs $ vars fun of 
        Right parsed -> case updateTable (storeDefinition parsedFun) (mapToEntry parsed) of
            Left err1 -> returnJson $ Result [err1]
            Right symbolTable -> case calculateZeroPoints symbolTable parsedFun of
                Right zp -> returnJson $ Result zp
                Left err2 -> returnJson $ Result [err2]
        Left err5 -> returnJson $ Result [show err5]
    Left err -> returnJson $ Result [show err]
