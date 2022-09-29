{-# LANGUAGE OverloadedStrings, BlockArguments #-}
module Handler.Zeropoints where

import Foundation ( Handler )
import Import.NoFoundation
    ( Value,
      requireInsecureJsonBody,
      returnJson )
import Data.Aeson()
import Yesod.Core.Content()
import Parser.Parser ( parseFunction )
import Parser.ZeroCrossings
    ( calculateZeroPoints, returnExpressionFromDef )
import Handler.JSONTypes
    ( ZeroPoints(ZeroPoints), Function(fnString) )

--------------------------------------------------------------------------------------------------------------------

{-
    POST Request, der ein JSON mit dem eingegebenen String entgegen nimmt und die Nullstellen und die geparste 
    Funktion also JSON zurückgibt
-}

postZeroR :: Handler Value
postZeroR = do
    fun <- requireInsecureJsonBody :: Handler Function
    case parseFunction $ fnString fun of 
        Left _ -> returnJson $ ZeroPoints ["error"]
        Right expr -> returnJson $ ZeroPoints (calculateZeroPoints $ returnExpressionFromDef expr)
    
    