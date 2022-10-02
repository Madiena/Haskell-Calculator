{-# LANGUAGE DeriveGeneric, BlockArguments #-}
module Handler.JSONTypes where

import Import
import Data.Aeson()
import Yesod.Core.Content()

newtype Function = Function {
    fnString :: String 
} deriving (Show, Generic)

data CalcIn = CalcIn {
    funString :: String,
    vars :: [String]
} deriving (Show, Generic)

newtype Result = Result {
    zeroPoints :: [String]
} deriving (Show, Generic)

newtype ResultDouble = ResultDouble {
    res :: Double
} deriving (Show, Generic)

instance ToJSON Result
instance FromJSON Result

instance ToJSON ResultDouble
instance FromJSON ResultDouble

instance ToJSON Function
instance FromJSON Function

instance ToJSON CalcIn
instance FromJSON CalcIn





    
    