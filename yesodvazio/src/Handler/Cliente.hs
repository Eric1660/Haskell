{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Cliente where

import Import
import Text.Cassius

formCliente :: Form Cliente
formCliente = renderDivs $ Cliente
    <$>areq textField (FieldSettings ""
                                   (Just "Nome")
                                   (Just "n1")
                                   Nothing
                                   [("class","formNome")]
                      ) Nothing
	
	<*>areq emailField (FieldSettings ""
                                    (Just "E-mail")
                                    (Just "n1")
                                    Nothing
                                    [("class","formNome")]
                        ) Nothing
	
	<*>areq textField (FieldSettings ""
                                    (Just "Endereço")
                                    (Just "n1")
                                    Nothing
                                    [("class","formNome")]
                        ) Nothing
						
    <*>areq intField (FieldSettings ""
                                    (Just "N°")
                                    (Just "n1")
                                    Nothing
                                    [("class","formNome")]
                        ) Nothing							
	
	<*>areq textareaField (FieldSettings ""
                                    (Just "Pedido")
                                    (Just "n1")
                                    Nothing
                                    [("class","formNome")]
                        ) Nothing

getClientR :: Handler Html
getClientR = do
    (widget,_) <- generateFormPost formCliente
    msg <- getMessage -- Handler (Maybe Text)
    defaultLayout $ do
		toWidgetHead $(cassiusFile "templates/home.cassius")
		$(whamletFile "templates/client.hamlet")
	{-
		[whamlet|
            $maybe mensa <- msg
                <h2>
                    ^{mensa}
    
            <form action=@{ClientR} method=post>
                ^{widget}
                <input type="submit" value="Enviar">
        |]
	-}
		

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
            E-mail: #{clienteEmail cliente}
        <h2>
            Endereço: #{clienteEndereco cliente}
        <h2>
            n°: #{clienteNumero cliente}
        <h2>
            Mensagem: #{clienteMensagem cliente}
   |]

postApagarClieR :: ClienteId -> Handler Html
postApagarClieR pid = do
    runDB $ delete pid
    redirect ListaClieR

getListaClieR :: Handler Html
getListaClieR = do
    clientes <- runDB $ selectList [] [Asc ClienteNome] 
    defaultLayout $ do
		$(whamletFile "templates/listarCliente.hamlet")
		toWidgetHead $(cassiusFile "templates/home.cassius")
