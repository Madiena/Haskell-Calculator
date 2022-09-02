{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Handler.Home where

import Import

getHomeR :: Handler ()
getHomeR = do 
    sendFile typeHtml "static/index.html"
    sendFile typeJavascript "static/assets/index.051e5e5a.js"

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes.yesodroutes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.

postHomeR :: Handler Html
postHomeR = defaultLayout $ do 
    setTitle "Hallo Welt"
    
