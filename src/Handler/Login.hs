{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Login where

import Import
import Text.Lucius

formLogin :: Form Usuario
formLogin = renderDivs $ Usuario 
    <$> areq textField "Nome: " Nothing
	<*> areq textField "Endereço: " Nothing
	<*> areq intField "N°: " Nothing
	<*> areq textField "E-mail: "  Nothing
    <*> areq passwordField "Senha:  "  Nothing

    
getAutR :: Handler Html
getAutR = do
    (widget,_) <- generateFormPost formLogin
    msg <- getMessage
    defaultLayout $ do
        usuario <- lookupSession "_ID"
        toWidgetHead $(luciusFile "templates/home.lucius")
        $(whamletFile "templates/login.hamlet")

postAutR :: Handler Html
postAutR = do
    ((result,_),_) <- runFormPost formLogin
    case result of
        FormSuccess (Usuario "Eric" "Rua da FATEC" 1 "root@root.com" "root") -> do
            setSession "_ID" "admin"
            redirect AdminR
        FormSuccess (Usuario nome endereco numero email senha) -> do
            usuarioExiste <- runDB $ getBy (UniqueEmail email)
            case usuarioExiste of
                Nothing -> do
                    setMessage [shamlet|
                        Usuario nao cadastrado
                    |]
                    redirect AutR
                Just (Entity _ usuario) -> do 
                    if senha == usuarioSenha usuario then do
                        setSession "_ID" (usuarioEmail usuario)
                        redirect HomeR
                    else do
                        setMessage [shamlet|
                            Usuario e/ou Senha nao conferem
                        |]
                        redirect AutR
        _ -> redirect HomeR
    
postSairR :: Handler Html
postSairR = do
    deleteSession "_ID"
    redirect HomeR
        
getAdminR :: Handler Html
getAdminR = do
    defaultLayout [whamlet|
        BEM-VINDO!
    |]