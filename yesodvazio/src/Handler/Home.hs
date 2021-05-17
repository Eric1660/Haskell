{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import
import Text.Lucius
import Text.Julius
import Network.HTTP.Types.Status
import Database.Persist.Postgresql

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        toWidgetHead $(juliusFile "templates/home.julius")
        toWidgetHead $(luciusFile "templates/home.lucius")
        $(whamletFile "templates/home.hamlet")

getPage1R :: Handler Html
getPage1R = do
    defaultLayout $ do
        toWidgetHead $(juliusFile "templates/home.julius")
        toWidgetHead $(luciusFile "templates/home.lucius")
        $(whamletFile "templates/page1r.hamlet")

getPage2R :: Handler Html
getPage2R = do
    defaultLayout $ do
        toWidgetHead $(juliusFile "templates/home.julius")
        toWidgetHead $(luciusFile "templates/home.lucius")
        $(whamletFile "templates/page2r.hamlet")