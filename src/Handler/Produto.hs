{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Produto where

import Import
import Text.Lucius
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

formProduto :: Form Produto
formProduto = renderDivs $ Produto 
    <$> areq textField (FieldSettings "Nome: " 
                                        Nothing
                                        (Just "h21")
                                        Nothing
                                        [("class", "minhaClasse")]) Nothing
    <*> areq doubleField "Preco: " Nothing

getProdutoR :: Handler Html
getProdutoR = do
     (formWidget, _) <- generateFormPost formProduto
     defaultLayout $ do
        addStylesheet (StaticR css_bootstrap_css)
        toWidgetHead $(luciusFile "templates/descr.lucius")
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
        $(whamletFile "templates/descr.hamlet")


getListaR :: Handler Html
getListaR = do
    produtos <- runDB $ selectList [] [Desc ProdutoValor]
    defaultLayout $ do
        $(whamletFile "templates/listar.hamlet")