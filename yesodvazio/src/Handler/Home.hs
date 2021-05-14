{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import
-- import Network.HTTP.Types.Status
-- import Database.Persist.Postgresql

-- defaultLayout converte um Widget (front) 
-- para Handler (back)
getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        toWidgetHead [julius|
            function ola(){
                alert("ola mundo!");
            }
        |]
        -- css/bootstrap.css
        addStylesheet (StaticR css_bootstrap_css)
        toWidgetHead [cassius|
            h1 
                color: red;
        |]
        [whamlet|
            <h1>
                OLA MUNDO
                
            <button onclick="ola()"> 
                Click
                
            <img src=@{StaticR img_fatec_png}>
        |]










