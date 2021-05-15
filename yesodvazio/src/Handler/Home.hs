{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import
import Text.Lucius
import Network.HTTP.Types.Status
import Database.Persist.Postgresql

getPage1R :: Handler Html
getPage1R = do
	defaultLayoult $ do
		addStylesheet (StaticR css_bootstrap_css)
		toWidgetHead $(luciusFile "templates/home.lucius")
		$(whamletFile "templates/page1r.hamlet")

getPage2R :: Handler Html
getPage2R = do
	defaultLayoult $ do
		addStylesheet (StaticR css_bootstrap_css)
		toWidgetHead $(luciusFile "templates/home.lucius")
		$(whamletFile "templates/page2r.hamlet")
		
getHomeR :: Handler Html
getHomeR = do
    defaultLayoult $ do
	 -- css/bootstrap.css
        addStylesheet (StaticR css_bootstrap_css)
        toWidgetHead $(luciusFile "templates/home.lucius")
        $(whamletFile "templates/home.hamlet")