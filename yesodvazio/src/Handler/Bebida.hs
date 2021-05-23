{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Bebida where

import Import
import Text.Cassius

formBebida :: Form Bebida
formBebida = renderDivs $ Bebida
    <$>areq textField (FieldSettings ""
                                   (Just "Nome")
                                   (Just "n1")
                                   Nothing
                                   [("class","formNome")]
                      ) Nothing
	
    <*> areq doubleField (FieldSettings "" 
                                      (Just "Preço:") 
                                      (Just "n1")
                                      Nothing
                                      [("class","formNome")]
                        ) Nothing
					   
    <*> areq textareaField (FieldSettings "" 
                                      (Just "Descrição:") 
                                      (Just "n1")
                                      Nothing
                                      [("class","formNome")]
                       ) Nothing

getBebidaR :: Handler Html
getBebidaR = do
    (widget,_) <- generateFormPost formBebida
    msg <- getMessage -- Handler (Maybe Text)
    defaultLayout $ do
	
		[whamlet|
            $maybe mensa <- msg
                <h2>
                    ^{mensa}
    
            <form action=@{BebidaR} method=post>
                ^{widget}
                <input type="submit" value="Cadastrar">
        |]
		

postBebidaR :: Handler Html
postBebidaR = do
    ((result,_),_) <- runFormPost formBebida
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
		$(whamletFile "templates/listarBebida.hamlet")
		toWidgetHead $(cassiusFile "templates/home.cassius")