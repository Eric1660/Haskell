{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Pizza where

import Import
import Text.Lucius

formPizza :: Form Pizza
formPizza = renderDivs $ Pizza
    <$>areq textField (FieldSettings ""
                                   (Just "Nome")
                                   (Just "n1")
                                   Nothing
                                   [("class","formNome")]
                      ) Nothing
	
	<*>areq textField  (FieldSettings ""
                                    (Just "Borda")
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

getPizzaR :: Handler Html
getPizzaR = do
    (widget,_) <- generateFormPost formPizza
    msg <- getMessage -- Handler (Maybe Text)
    defaultLayout $ do
        toWidgetHead $(luciusFile "templates/home.lucius")
        $(whamletFile "templates/pizza.hamlet")
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

postPizzaR :: Handler Html
postPizzaR = do
    ((result,_),_) <- runFormPost formPizza
    case result of
         FormSuccess pizza -> do
             runDB $ insert pizza
             setMessage [shamlet|
                    <div>
                        Pizza Inserido com Sucesso
             |]
             redirect PizzaR
         _ -> redirect HomeR
		 
getMamamiaR :: PizzaId -> Handler Html
getMamamiaR pid = do
    pizza <- runDB $ get404 pid 
    defaultLayout [whamlet|
        <h2>
            Nome: #{pizzaNome pizza}
        <h2>
            Borda: #{pizzaBorda pizza}
        <h2>
            Preço: #{pizzaPreco pizza}
        <h2>
            Descrição: #{pizzaDesc pizza}
   |]

getListaPizzaR :: Handler Html
getListaPizzaR = do
    pizzas <- runDB $ selectList [] [Asc PizzaNome] 
    defaultLayout $ do
		$(whamletFile "templates/listarPizza.hamlet")
		toWidgetHead $(luciusFile "templates/home.lucius")