{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Usuario where

import Import
import Text.Lucius

formLogin :: Form (Usuario, Text)
formLogin = renderBootstrap $ (,)
        <$> (Usuario 
            <$> areq textField "E-mail: " Nothing
            <*> areq passwordField "Senha:  " Nothing
            )
        <*> areq passwordField  "Confirmação da Senha: " Nothing
    
getUsuarioR :: Handler Html
getUsuarioR = do
    (widget,_) <- generateFormPost formLogin
    msg <- getMessage
    defaultLayout $ do
        usuario <- lookupSession "_ID"
        toWidgetHead $(luciusFile "templates/home.lucius")
        $(whamletFile "templates/usuario.hamlet")

postUsuarioR :: Handler Html
postUsuarioR = do
    ((result,_),_) <- runFormPost formLogin
    case result of
        FormSuccess (usuario@(Usuario email senha), conf) -> do
            usuarioExiste <- runDB $ getBy (UniqueEmail email)
            case usuarioExiste of
                 Just _ -> do
                     setMessage [shamlet|
                            <div>
                                E-MAIL JA CADASTRADO!
                        |]
                     redirect UsuarioR
                 Nothing -> do
                    if senha == conf then do
                        runDB $ insert usuario
                        setMessage [shamlet|
                            <div>
                                Usuario INSERIDO COM SUCESSO!
                        |]
                        redirect UsuarioR
                    else do
                        setMessage [shamlet|
                            <div>
                                SENHA E CONFIRMACAO DIFERENTES!
                        |]
                        redirect UsuarioR
        _ -> redirect HomeR
