{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Bebes where

import Import
import Text.Lucius
import Database.Persist.Postgresql

formBebes :: UsuarioId -> Form Bebes
formBebes pid = renderDivs $ Bebes
    <$> pure pid
    <*> areq (selectField bebidaCB) "Bebida: " Nothing
    <*> areq intField "Quantidade: " Nothing

bebidaCB :: Handler (OptionList (Key Bebida))
bebidaCB = do
  bebidas <- runDB $ selectList [] [Asc BebidaNome]
  optionsPairs $ 
      map (\r -> (bebidaNome $ entityVal r, entityKey r)) bebidas

getBebesR :: UsuarioId -> Handler Html
getBebesR pid = do
    (widget,_) <- generateFormPost (formBebes pid)
    msg <- getMessage
    defaultLayout $ do
        usuario <- lookupSession "_ID"
        toWidgetHead $(luciusFile "templates/home.lucius")
        $(whamletFile "templates/bebes.hamlet")

postBebesR :: UsuarioId -> Handler Html
postBebesR pid = do
    ((result,_),_) <- runFormPost (formBebes pid)
    case result of
         FormSuccess bebes -> do
             runDB $ insert bebes
             setMessage [shamlet|
                    <div>
                        Pedido registrado com sucesso
             |]
             redirect (AdagaR pid)
         _ -> redirect HomeR

mult :: Double -> Double -> Double
mult = (*)

getAdagaR :: UsuarioId -> Handler Html
getAdagaR pid = do 
    let sql = "SELECT ??,??,?? FROM bebida \
          \ INNER JOIN bebes ON bebes.bebid = bebida.id \
          \ INNER JOIN usuario ON bebes.userid = usuario.id \
          \ WHERE usuario.id = ?"
    usuario <- runDB $ get404 pid
    tudo <- runDB $ rawSql sql [toPersistValue pid] :: Handler [(Entity Bebida,Entity Bebes,Entity Usuario)]
    defaultLayout $ do 
        toWidgetHead $(luciusFile "templates/home.lucius")
        [whamlet|
            <body class="JOIN">                
                <h1 class="JOIN">
                    Carrinho de #{usuarioNome usuario}
                <ul class="JOIN">
                    $forall (Entity _ bebida, Entity _ bebes, Entity _ _) <- tudo
                        <li class="JOIN2">
                            #{bebidaNome bebida}, #{mult (bebidaPreco bebida)(fromIntegral (bebesQtbebida bebes))}
                <a href=@{HomeR}>	
                    <input class="button" type="submit" value="Voltar">
        |]