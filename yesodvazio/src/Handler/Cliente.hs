{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Cliente where

import Import
import Text.Lucius

formCliente :: Form Cliente
formCliente = renderDivs $ Cliente
    <$>areq textField "Nome: " Nothing
    <*>areq textField "Endereço: " Nothing
    <*>areq intField "N°: " Nothing
    <*>areq textareaField "Pedido: " Nothing

getClientR :: Handler Html
getClientR = do
    (widget,_) <- generateFormPost formCliente
    msg <- getMessage
    defaultLayout $ do
        usuario <- lookupSession "_ID"
        toWidgetHead $(luciusFile "templates/home.lucius")
        $(whamletFile "templates/client.hamlet")

postClientR :: Handler Html
postClientR = do
    ((result,_),_) <- runFormPost formCliente
    case result of
         FormSuccess cliente -> do
             runDB $ insert cliente
             setMessage [shamlet|
                    <div>
                        Pedido Enviado com sucesso
             |]
             redirect ClientR
         _ -> redirect HomeR

getPedidoR :: ClienteId -> Handler Html
getPedidoR pid = do
    cliente <- runDB $ get404 pid 
    defaultLayout [whamlet|
        <h2>
            Nome: #{clienteNome cliente}
        <h2>
            Endereço: #{clienteEndereco cliente}
        <h2>
            N°: #{clienteNumero cliente}
        <h2>
            Pedido: #{clienteMensagem cliente}
   |]

postApagarClieR :: ClienteId -> Handler Html
postApagarClieR pid = do
    runDB $ delete pid
    redirect ListaClieR

getListaClieR :: Handler Html
getListaClieR = do
    clientes <- runDB $ selectList [] [Asc ClienteNome] 
    defaultLayout $ do
        usuario <- lookupSession "_ID"
        toWidgetHead $(luciusFile "templates/home.lucius")
        $(whamletFile "templates/listarCliente.hamlet")