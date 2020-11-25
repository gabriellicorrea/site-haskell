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
import Tools
formProduto :: Maybe Produto -> Form Produto
formProduto mp = renderDivs $ Produto 
    <$> areq textField (FieldSettings "Nome: " 
                                        Nothing
                                        (Just "h21")
                                        Nothing
                                        [("class", "minhaClasse")]) 
                                        (fmap produtoNome mp)
    <*> areq doubleField "Preco: " (fmap produtoValor mp)

auxProdutoR :: Route App -> Maybe Produto -> Handler Html
auxProdutoR rt mp = do
     (formWidget, _) <- generateFormPost (formProduto mp)
     defaultLayout $ do
        addStylesheet (StaticR css_bootstrap_css)
        toWidgetHead $(luciusFile "templates/descr.lucius")
        [whamlet|
        
            <form action=@{rt} method=post>
                ^{formWidget}
                <input type="submit"  value="OK">
        |]


getProdutoR :: Handler Html
getProdutoR = auxProdutoR ProdutoR Nothing


postProdutoR :: Handler Html
postProdutoR = do
    ((result, _), _) <- runFormPost (formProduto Nothing)
    case result of
         FormSuccess produto -> do
            pid <- runDB $ insert produto
            redirect (DescR pid) 
         _ -> redirect HomeR


getDescR :: ProdutoId -> Handler Html
getDescR pid = do
    produto <- runDB $ get404 pid
    (widget, _) <- generateFormPost formQt
    defaultLayout $ do
        $(whamletFile "templates/descr.hamlet")


getListaR :: Handler Html
getListaR = do
    produtos <- runDB $ selectList [] [Desc ProdutoValor]
    defaultLayout $ do
        toWidgetHead $(luciusFile "templates/listar.lucius")
        $(whamletFile "templates/listar.hamlet")    
        


getUpdProdR :: ProdutoId -> Handler Html
getUpdProdR pid = do
    antigo <- runDB $ get404 pid
    auxProdutoR (UpdProdR pid) (Just antigo)

postUpdProdR :: ProdutoId -> Handler Html
postUpdProdR pid = do
    ((result, _), _) <- runFormPost (formProduto Nothing)
    case result of
         FormSuccess produto -> do
            pid <- runDB $ replace pid produto
            redirect ListaR 
         _ -> redirect HomeR


postDelProdR :: ProdutoId -> Handler Html
postDelProdR pid = do
    runDB $ delete pid
    redirect ListaR