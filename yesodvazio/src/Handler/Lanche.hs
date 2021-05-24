{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Lanche where

import Import
import Text.Lucius


formLanche :: Form Lanche
formLanche = renderDivs $ Lanche
    <$> areq textField (FieldSettings "" 
                                      (Just "Nome do produto:") 
                                      (Just "n1")
                                      Nothing
                                      [("class","formNome")]
                       ) Nothing
    <*> areq textField (FieldSettings "" 
                                      (Just "Pão:") 
                                      (Just "n1")
                                      Nothing
                                      [("class","formNome")]
                       ) Nothing
    <*> areq textField (FieldSettings "" 
                                      (Just "Carne:") 
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

getLancheR :: Handler Html
getLancheR = do
    (widget,_) <- generateFormPost formLanche
    msg <- getMessage
    defaultLayout $ do
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
    ((result,_),_) <- runFormPost formLanche
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
		$(whamletFile "templates/listarLanche.hamlet")
		toWidgetHead $(luciusFile "templates/home.lucius")