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
import Handler.Helpers
import Service.ZeroCrossings
import Service.SymbolTable
import Service.Parser
import Yesod.Core.Content ()

--------------------------------------------------------------------------------------------------------------------

{-
    POST Request, der die Nullstellen zu einer gegebenen Funktion unter Beachtung von Variablen berechnet und als 
    Array zurückgibt
-}
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
