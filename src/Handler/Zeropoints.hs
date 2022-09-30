{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Zeropoints where

import Data.Aeson ()
import Foundation (Handler)
import Handler.JSONTypes
  ( CalcIn (funString, vars),
    ZeroPoints (ZeroPoints),
  )
import Import.NoFoundation
  ( Default (def),
    Value,
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

parseDefs :: [String] -> Either ParseError [Definition]
parseDefs li = [parseDefinition d | d <- li, isRight $ parseDefinition d]

postZeroR :: Handler Value
postZeroR = do
  fun <- requireInsecureJsonBody :: Handler CalcIn
  case parseDefinition $ funString fun of
    Right parsedFun -> case parseDefs $ vars fun of 
        Right parsed -> case updateTable (storeDefinition parsedFun) (mapToEntry parsed) of
            Left err1 -> returnJson $ ZeroPoints [err1]
            Right symbolTable -> case calculateZeroPoints symbolTable parsedFun of
                Right zp -> returnJson $ ZeroPoints zp
                Left err2 -> returnJson $ ZeroPoints [err2]
        Left err5 -> returnJson $ ZeroPoints [show err5]
    Left err -> returnJson $ ZeroPoints [show err]
