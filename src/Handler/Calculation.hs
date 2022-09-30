
{-# LANGUAGE OverloadedStrings, BlockArguments #-}
module Handler.Calculation where

import Foundation ( Handler )
import Import.NoFoundation
    ( Value,
      requireInsecureJsonBody,
      returnJson )
import Data.Aeson()
import Yesod.Core.Content()
import Service.Parser ( parseDefinition )
import Service.Compiler ( compileToJS )
import Handler.JSONTypes ( Function(..) )

--------------------------------------------------------------------------------------------------------------------

{-
    POST Request, der den eingegebenen String entgegen nimmt und den Kompilierten JavaScript Code als String
    zurückgibt
-}
postCalcR :: Handler Value
postCalcR = do
    fun <- requireInsecureJsonBody :: Handler Function
    case parseDefinition (fnString fun) of
       Left _ -> returnJson $ Function "error"
       Right expr -> returnJson $ Function (compileToJS expr)