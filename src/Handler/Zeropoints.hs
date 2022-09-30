{-# LANGUAGE OverloadedStrings, BlockArguments #-}
module Handler.Zeropoints where

import Foundation ( Handler )
import Import.NoFoundation
    ( Value,
      requireInsecureJsonBody,
      returnJson, Default (def) )
import Data.Aeson()
import Yesod.Core.Content()
import Service.Parser ( parseDefinition )
import Service.ZeroCrossings
    ( calculateZeroPoints)
import Handler.JSONTypes
    ( ZeroPoints(ZeroPoints), CalcIn (funString, vars) )
import Service.SymbolTable (SymbolTable, storeDefinition, updateTable)
import Service.AbstractSyntax ( Definition )
import Text.Parsec (ParseError)

--------------------------------------------------------------------------------------------------------------------

{-
    POST Request, der ein JSON mit dem eingegebenen String entgegen nimmt und die Nullstellen und die geparste 
    Funktion also JSON zurückgibt
-}
mapToEntry :: [Either ParseError Definition] -> Either ParseError SymbolTable
mapToEntry li = [ case d of 
                    Right def -> storeDefinition def
                    Left err -> err 
                | d <- li]

parseDefs :: [String] -> [Either ParseError Definition]
parseDefs li = [parseDefinition d | d <- li]

postZeroR :: Handler Value
postZeroR = do
    fun <- requireInsecureJsonBody :: Handler CalcIn
    parsedFun <- case parseDefinition $ funString fun of
        Right def -> def :: Handler Definition
        Left err -> returnJson $ ZeroPoints [err]
    symbolTable <-  case updateTable (storeDefinition parsedFun) (mapToEntry $ parseDefs $ vars fun) of 
        Right tab -> tab
        Left err -> returnJson ZeroPoints [err]
    returnJson $ ZeroPoints $ calculateZeroPoints symbolTable parsedFun

