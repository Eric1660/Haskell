{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import
import Text.Cassius
import Network.HTTP.Types.Status
import Database.Persist.Postgresql

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
		$(whamletFile "templates/home.hamlet")
		toWidgetHead $(cassiusFile "templates/home.cassius")