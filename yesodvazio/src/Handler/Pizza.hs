{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Pizza where

import Import
import Text.Lucius
import Handler.Auxiliar

formPizza :: Maybe Pizza -> Form Pizza
formPizza mc = renderDivs $ Pizza
    <$>areq textField "Nome: "           (fmap pizzaNome mc)
	<*>areq textField "Borda: "          (fmap pizzaBorda mc)
    <*> areq doubleField "Preço(R$): "   (fmap pizzaPreco mc)  
    <*> areq textareaField "Descrição: " (fmap pizzaDesc mc)
getPizzaR :: Handler Html
getPizzaR = do
    (widget,_) <- generateFormPost (formPizza Nothing)
    msg <- getMessage -- Handler (Maybe Text)
    defaultLayout $ do
        usuario <- lookupSession "_ID"
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
    ((result,_),_) <- runFormPost (formPizza Nothing)
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
        usuario <- lookupSession "_ID"
        toWidgetHead $(luciusFile "templates/home.lucius")
        $(whamletFile "templates/listarPizza.hamlet")

getEditarPizzaR :: PizzaId -> Handler Html
getEditarPizzaR pid = do 
    pizza <- runDB $ get404 pid
    (widget,_) <- generateFormPost (formPizza (Just pizza))
    msg <- getMessage
    defaultLayout (formWidget widget msg (EditarPizzaR pid) "Editar")
        

postEditarPizzaR :: PizzaId -> Handler Html
postEditarPizzaR pid = do
    _ <- runDB $ get404 pid 
    ((result,_),_) <- runFormPost (formPizza Nothing)
    case result of 
         FormSuccess novoPizza -> do
             runDB $ replace pid novoPizza
             redirect ListaPizzaR
         _ -> redirect HomeR