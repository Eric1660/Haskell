{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Italiano where

import Import
import Text.Lucius
import Database.Persist.Postgresql

formItaliano :: UsuarioId -> Form Italiano
formItaliano pid = renderDivs $ Italiano
    <$> pure pid
    <*> areq (selectField pizzaCB) "Pizza: " Nothing
    <*> areq intField "Quantidade: " Nothing
    <*> lift (liftIO (map utctDay getCurrentTime))

pizzaCB :: Handler (OptionList (Key Pizza))
pizzaCB = do
  pizzas <- runDB $ selectList [] [Asc PizzaNome]
  optionsPairs $ 
      map (\r -> (pizzaNome $ entityVal r, entityKey r)) pizzas

getCompraR :: UsuarioId -> Handler Html
getCompraR pid = do
    (widget,_) <- generateFormPost (formItaliano pid)
    msg <- getMessage
    defaultLayout $ do
        usuario <- lookupSession "_ID"
        toWidgetHead $(luciusFile "templates/home.lucius")
        $(whamletFile "templates/italiano.hamlet")

postCompraR :: UsuarioId -> Handler Html
postCompraR pid = do
    ((result,_),_) <- runFormPost (formItaliano pid)
    case result of
         FormSuccess italiano -> do
             runDB $ insert italiano
             setMessage [shamlet|
                    <div>
                        Pedido registrado com sucesso
             |]
             redirect (FornoR pid)
         _ -> redirect HomeR

mult :: Double -> Double -> Double
mult = (*)

getFornoR :: UsuarioId -> Handler Html
getFornoR pid = do 
    let sql = "SELECT ??,??,?? FROM pizza \
          \ INNER JOIN italiano ON italiano.pizid = italiano.id \
          \ INNER JOIN usuario ON italiano.userid = usuario.id \
          \ WHERE usuario.id = ?"
    usuario <- runDB $ get404 pid
    tudo <- runDB $ rawSql sql [toPersistValue pid] :: Handler [(Entity Pizza,Entity Italiano,Entity Usuario)]
    defaultLayout $ do 
        toWidgetHead $(luciusFile "templates/home.lucius")
        [whamlet|
            <body class="JOIN">
                <h1 class="JOIN">
                    Carrinho de #{usuarioNome usuario}
                <ul class="JOIN">
                    $forall (Entity _ pizza, Entity _ italiano, Entity _ _) <- tudo
                        <li class="JOIN2">
                            #{pizzaNome pizza}, #{mult (pizzaPreco pizza)(fromIntegral (italianoQtpizza italiano))} no dia #{show $ italianoDias italiano}
                <a href=@{HomeR}>	
                    <input class="button" type="submit" value="Voltar">
        |]