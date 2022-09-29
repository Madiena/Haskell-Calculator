{-# LANGUAGE OverloadedStrings, BlockArguments #-}
module Handler.Home where

import Foundation ( Handler )
import Import.NoFoundation ( typeHtml, sendFile )
import Data.Aeson()
import Yesod.Core.Content()

--------------------------------------------------------------------------------------------------------------------

{-
    serviert das Frontend
-}

getHomeR :: Handler ()
getHomeR = do sendFile typeHtml "static/index.html"