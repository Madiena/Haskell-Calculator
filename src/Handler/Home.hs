{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Handler.Home where

import Import

getHomeR :: Handler ()
getHomeR = do 
    sendFile typeHtml "static/index.html"


