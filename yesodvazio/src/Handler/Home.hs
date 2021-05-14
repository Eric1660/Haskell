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

getPage1R :: Handler Html
getPage1R = do
	defaultLayoult $ do
		addStylesheet (StaticR css_bootstrap_css)
		toWidgetHead $(cassiusFile "templates/home.cassius")
		$(whamletFile "templates/page1r.hamlet")

getPage2R :: Handler Html
getPage2R = do
	defaultLayoult $ do
		addStylesheet (StaticR css_bootstrap_css)
		toWidgetHead $(cassiusFile "templates/home.cassius")
		$(whamletFile "templates/page2r.hamlet")
		

menu :: [(Route App, Text)] -> Widget
menu [] = [whamlet| <h1> ERRO GRAVE |]

getHomeR :: Handler Html
getHomeR = do
    defaultLayoult $ do
	 -- css/bootstrap.css
        addStylesheet (StaticR css_bootstrap_css)
        toWidgetHead $(cassiusFile "templates/home.cassius")
        $(whamletFile "templates/home.hamlet")

        
        
