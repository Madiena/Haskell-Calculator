
{-# LANGUAGE OverloadedStrings, BlockArguments #-}
module Handler.Calculation where

import Foundation ( Handler )
import Import.NoFoundation
    ( Value,
      requireInsecureJsonBody,
      returnJson )
import Data.Aeson()
import Yesod.Core.Content()
import Service.Parser ( parseDefinition, returnExpressionFromDef )
import Text.Parsec
import Service.AbstractSyntax
import Service.SymbolTable
import Service.Calculation ( calculateExp )
import Handler.JSONTypes
import Data.Either

--------------------------------------------------------------------------------------------------------------------

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

{-
    POST Request, der den eingegebenen String entgegen nimmt und den Kompilierten JavaScript Code als String
    zurückgibt
-}
postCalcR :: Handler Value
postCalcR = do
  fun <- requireInsecureJsonBody :: Handler CalcIn
  case parseDefinition $ funString fun of
    Right parsedFun -> case parseDefs $ vars fun of 
        Right parsed -> case updateTable (storeDefinition parsedFun) (mapToEntry parsed) of
            Left err1 -> returnJson $ Result [err1]
            Right symbolTable -> case calculateExp (returnExpressionFromDef parsedFun) symbolTable of
                Right result -> returnJson $ ResultDouble result
                Left err2 -> returnJson $ Result [err2]
        Left err5 -> returnJson $ Result [show err5]
    Left err -> returnJson $ Result [show err]