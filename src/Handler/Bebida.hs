{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Bebida where

import Import
import Text.Lucius
import Handler.Auxiliar

formBebida :: Maybe Bebida -> Form Bebida
formBebida mc = renderDivs $ Bebida
    <$> areq textField "Nome: "          (fmap bebidaNome mc)
    <*> areq doubleField "Preço(R$): "   (fmap bebidaPreco mc)
    <*> areq textareaField "Descrição: " (fmap bebidaDesc mc)

getBebidaR :: Handler Html
getBebidaR = do
    (widget,_) <- generateFormPost (formBebida Nothing)
    msg <- getMessage
    defaultLayout $ do
        usuario <- lookupSession "_ID"
        toWidgetHead $(luciusFile "templates/home.lucius")
        $(whamletFile "templates/bebida.hamlet")

postBebidaR :: Handler Html
postBebidaR = do
    ((result,_),_) <- runFormPost (formBebida Nothing)
    case result of
         FormSuccess bebida -> do
             runDB $ insert bebida
             setMessage [shamlet|
                    <div>
                        Bebida Inserido com Sucesso
             |]
             redirect BebidaR
         _ -> redirect HomeR

getDrinkR :: BebidaId -> Handler Html
getDrinkR pid = do
    bebida <- runDB $ get404 pid 
    defaultLayout [whamlet|
        <h2>
            Nome: #{bebidaNome bebida}
        <h2>
            Preço: #{bebidaPreco bebida}
        <h2>
            Descrição: #{bebidaDesc bebida}
   |]

getListaBebidaR :: Handler Html
getListaBebidaR = do
    bebidas <- runDB $ selectList [] [Asc BebidaNome] 
    defaultLayout $ do
        usuario <- lookupSession "_ID"
        toWidgetHead $(luciusFile "templates/home.lucius")
        $(whamletFile "templates/listarBebida.hamlet")

postApagarBebidaR :: BebidaId -> Handler Html
postApagarBebidaR pid = do
    runDB $ delete pid
    redirect ListaBebidaR

getEditarBebidaR :: BebidaId -> Handler Html
getEditarBebidaR pid = do 
    bebida <- runDB $ get404 pid
    (widget,_) <- generateFormPost (formBebida (Just bebida))
    msg <- getMessage
    defaultLayout (formWidget widget msg (EditarBebidaR pid) "Editar")
        

postEditarBebidaR :: BebidaId -> Handler Html
postEditarBebidaR pid = do
    _ <- runDB $ get404 pid 
    ((result,_),_) <- runFormPost (formBebida Nothing)
    case result of 
         FormSuccess novoBebida -> do
             runDB $ replace pid novoBebida
             redirect ListaBebidaR
         _ -> redirect HomeR