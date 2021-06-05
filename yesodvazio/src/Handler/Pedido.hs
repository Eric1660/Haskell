{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Pedido where

import Import
import Text.Lucius
import Database.Persist.Postgresql

formPedido :: UsuarioId -> Form Pedido
formPedido pid = renderDivs $ Pedido
    <$> pure pid
    <*> areq (selectField lancheCB) "Lanche: " Nothing
    <*> areq intField "Quantidade: " Nothing

lancheCB :: Handler (OptionList (Key Lanche))
lancheCB = do
  lanches <- runDB $ selectList [] [Asc LancheNome]
  optionsPairs $ 
      map (\r -> (lancheNome $ entityVal r, entityKey r)) lanches

getEncomendaR :: UsuarioId -> Handler Html
getEncomendaR pid = do
    (widget,_) <- generateFormPost (formPedido pid)
    msg <- getMessage
    defaultLayout $ do
        usuario <- lookupSession "_ID"
        toWidgetHead $(luciusFile "templates/home.lucius")
        $(whamletFile "templates/pedido.hamlet")

postEncomendaR :: UsuarioId -> Handler Html
postEncomendaR pid = do
    ((result,_),_) <- runFormPost (formPedido pid)
    case result of
         FormSuccess pedido -> do
             runDB $ insert pedido
             setMessage [shamlet|
                    <div>
                        Pedido registrado com sucesso
             |]
             redirect (CarrinhoR pid)
         _ -> redirect HomeR

mult :: Double -> Double -> Double
mult = (*)

getCarrinhoR :: UsuarioId -> Handler Html
getCarrinhoR pid = do 
    let sql = "SELECT ??,??,?? FROM lanche \
          \ INNER JOIN pedido ON pedido.lanid = pedido.id \
          \ INNER JOIN usuario ON pedido.userid = usuario.id \
          \ WHERE usuario.id = ?"
    usuario <- runDB $ get404 pid
    tudo <- runDB $ rawSql sql [toPersistValue pid] :: Handler [(Entity Lanche,Entity Pedido,Entity Usuario)]
    defaultLayout $ do 
        [whamlet|
            <h1>
                Carrinho de #{usuarioNome usuario}
            <ul>
                $forall (Entity _ lanche, Entity _ pedido, Entity _ _) <- tudo
                    <li>
                        #{lancheNome lanche}, #{mult (lanchePreco lanche)(fromIntegral (pedidoQtlanche pedido))}
        |]