{-# LANGUAGE DeriveGeneric, BlockArguments #-}
module Handler.JSONTypes where

import Import
import Data.Aeson()
import Yesod.Core.Content()

newtype Function = Function {
    fnString :: String 
} deriving (Show, Generic)

newtype ZeroPoints = ZeroPoints {
    zeroPoints :: [String]
} deriving (Show, Generic)


instance ToJSON ZeroPoints
instance FromJSON ZeroPoints

instance ToJSON Function
instance FromJSON Function




    
    