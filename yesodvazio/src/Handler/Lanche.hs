{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Lanche where

import Import
import Text.Lucius
import Handler.Auxiliar

formLanche :: Maybe Lanche -> Form Lanche
formLanche mc = renderDivs $ Lanche
    <$> areq textField "Nome: "          (fmap lancheNome mc)
    <*> areq textField "Pão: "           (fmap lanchePao mc)
    <*> areq textField "Carne: "         (fmap lancheCarne mc)
    <*> areq doubleField "Preço(R$): "   (fmap lanchePreco mc)	
    <*> areq textareaField "Descrição: " (fmap lancheDescr mc)

getLancheR :: Handler Html
getLancheR = do
    (widget,_) <- generateFormPost (formLanche Nothing)
    msg <- getMessage
    defaultLayout $ do
        usuario <- lookupSession "_ID"
        toWidgetHead $(luciusFile "templates/home.lucius")
        $(whamletFile "templates/lanche.hamlet")
	{-	
		[whamlet|
            $maybe mensa <- msg
                <h2>
                    ^{mensa}
    
            <form action=@{LancheR} method=post>
                ^{widget}
                <input type="submit" value="Cadastrar">
        |]
		
    -}
postLancheR :: Handler Html
postLancheR = do
    ((result,_),_) <- runFormPost (formLanche Nothing)
    case result of
         FormSuccess lanche -> do
             runDB $ insert lanche
             setMessage [shamlet|
                    <div>
                        LANCHE INSERIDO COM SUCESSO
             |]
             redirect LancheR
         _ -> redirect HomeR

getSandwichR :: LancheId -> Handler Html
getSandwichR pid = do
    lanche <- runDB $ get404 pid 
    defaultLayout [whamlet|
        <h2>
            Nome: #{lancheNome lanche}
        <h2>
            Pão: #{lanchePao lanche}
        <h2>
            Carne: #{lancheCarne lanche}
        <h2>
            Preco: #{lanchePreco lanche}
        <h2>
            Descr: #{lancheDescr lanche}
   |]

getListaLancheR :: Handler Html
getListaLancheR = do
    lanches <- runDB $ selectList [] [Asc LancheNome] 
    defaultLayout $ do
        usuario <- lookupSession "_ID"
        toWidgetHead $(luciusFile "templates/home.lucius")
        $(whamletFile "templates/listarLanche.hamlet")
		
getEditarLancheR :: LancheId -> Handler Html
getEditarLancheR pid = do 
    lanche <- runDB $ get404 pid
    (widget,_) <- generateFormPost (formLanche (Just lanche))
    msg <- getMessage
    defaultLayout (formWidget widget msg (EditarLancheR pid) "Editar")
        

postEditarLancheR :: LancheId -> Handler Html
postEditarLancheR pid = do
    _ <- runDB $ get404 pid 
    ((result,_),_) <- runFormPost (formLanche Nothing)
    case result of 
         FormSuccess novoLanche -> do
             runDB $ replace pid novoLanche
             redirect ListaLancheR
         _ -> redirect HomeR