{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Handler.Home where

import Import
import Data.Aeson()
import Yesod.Core.Content()

getHomeR :: Handler ()
getHomeR = do sendFile typeHtml "static/index.html"

newtype Function = Function {
    function :: Text 
} deriving (Show, Generic)
getFuction :: Function -> Text 
getFuction (Function fun) = fun 

instance ToJSON Function
instance FromJSON Function

postFunctionR :: Handler Value
postFunctionR = do
    fun <- requireInsecureJsonBody :: Handler Value
    returnJson fun
