{-# LANGUAGE DeriveGeneric, OverloadedStrings, BlockArguments #-}
--{-# LANGUAGE QuasiQuotes       #-}
module Handler.Home where

import Import
import Data.Aeson()
import Yesod.Core.Content()
import Parser.Parser

getHomeR :: Handler ()
getHomeR = do sendFile typeHtml "static/index.html"

newtype Function = Function {
    fnString :: String 
} deriving (Show, Generic)

instance ToJSON Function
instance FromJSON Function

postFunctionR :: Handler Value
postFunctionR = do
    fun <- requireInsecureJsonBody :: Handler Function
    case parseFunction (fnString fun) of
       Left err -> returnJson $ Function "error"
       Right expr -> returnJson $ Function (show expr)

    --returnJson fun
