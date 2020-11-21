{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Produto where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

formProduto :: Form Produto
formProduto = renderBootstrap3 BootstrapBasicForm $ Produto 
    <$> areq textField "Nome: " Nothing
    <*> areq doubleField "Preco: " Nothing

getProdutoR :: Handler Html
getProdutoR = do
     (formWidget, _) <- generateFormPost formProduto
     defaultLayout $ do
        [whamlet|
            <form action=@{ProdutoR} method=post>
                ^{formWidget}
                <input type="submit"  value="OK">
        |]


postProdutoR :: Handler Html
postProdutoR = do
    ((result, _), _) <- runFormPost formProduto
    case result of
         FormSuccess produto -> do
            pid <- runDB $ insert produto
            redirect (DescR pid)
         _ -> redirect HomeR

getDescR :: ProdutoId -> Handler Html
getDescR pid = do
    produto <- runDB $ get404 pid
    defaultLayout $ do
        [whamlet|
                <ul>
                    <li> Nome: #{produtoNome produto}
                    <li> Preco: #{produtoValor produto}
        |]