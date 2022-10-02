
{-# LANGUAGE OverloadedStrings, BlockArguments #-}
module Handler.Calculation where

import Foundation ( Handler )
import Import.NoFoundation
    ( Value,
      requireInsecureJsonBody,
      returnJson )
import Data.Aeson()
import Yesod.Core.Content()
import Service.SymbolTable
import Service.Calculation
import Service.Parser
import Text.Parsec
import Handler.Helpers
import Handler.JSONTypes
import Data.Either

--------------------------------------------------------------------------------------------------------------------

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