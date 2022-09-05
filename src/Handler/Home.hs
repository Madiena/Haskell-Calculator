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
    fun <- requireInsecureJsonBody :: Handler Function
--    handleFunction fun
    returnJson fun

--handleFunction :: Function -> Handler Value
--handleFunction f = 
    --das ist die Funktion, in der die eingegebene Funktion an den Parser kommt und die ganze Logik passiert

