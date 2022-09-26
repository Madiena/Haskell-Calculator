{-# LANGUAGE DeriveGeneric, OverloadedStrings, BlockArguments #-}
module Handler.Home where

import Import
import Data.Aeson()
import Yesod.Core.Content()
import Parser.Parser
import Parser.ZeroCrossings

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
       Left _ -> returnJson $ Function "error"
       Right expr -> returnJson $ Function (compileToJS expr)

data ZeroPoints = ZeroPoints {
    func :: String,
    zeroPoints :: [String]
} deriving (Show, Generic)
instance ToJSON ZeroPoints
instance FromJSON ZeroPoints

postZeroR :: Handler Value
postZeroR = do
    fun <- requireInsecureJsonBody :: Handler Function
    case parseFunction $ fnString fun of 
        Left _ -> returnJson $ ZeroPoints "error" ["error"]
        Right expr -> returnJson $ ZeroPoints (compileToJS expr) (calculateZeroPoints $ returnExpressionFromDef expr)
    
    