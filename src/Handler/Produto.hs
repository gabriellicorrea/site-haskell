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
import Database.Persist.Sql

formProduto :: Maybe Produto -> Form Produto
formProduto mp = renderBootstrap3 BootstrapBasicForm $ Produto 
    <$> areq textField (FieldSettings "Nome: " 
                                        Nothing
                                        (Just "h21")
                                        Nothing
                                        [("class", "form-control")]) 
                                        (fmap produtoNome mp)
    <*> areq doubleField (FieldSettings "Preço: " 
                                        Nothing
                                        (Just "h21")
                                        Nothing
                                        [("class", "form-control")]) 
                                        (fmap produtoValor mp)

auxProdutoR :: Route App -> Maybe Produto -> Handler Html
auxProdutoR rt mp = do
     (formWidget, _) <- generateFormPost (formProduto mp)
     defaultLayout $ do
        setTitle "Nova tarefa - Cadastrar produto"
        addStylesheet (StaticR css_bootstrapmin_css)
        addStylesheet (StaticR css_styles_css)
    
        toWidgetHead $(luciusFile "templates/descr.lucius")
        [whamlet|
            <h1 class="text-center mt-5">Adicionar produto</h1>
            <form action=@{rt} method=post class="mt-5">
                ^{formWidget}
                <input type="submit" class="btn btn-lg btn-primary btn-block" value="Salvar">
        |]
        addScriptRemote "https://code.jquery.com/jquery-3.5.1.slim.min.js"
        addScriptRemote "https://cdn.jsdelivr.net/npm/bootstrap@4.5.3/dist/js/bootstrap.bundle.min.js"


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
        setTitle "Nova tarefa - Adicionar nas minhas compras"
        addStylesheet (StaticR css_bootstrapmin_css)
        addStylesheet (StaticR css_styles_css) 
        $(whamletFile "templates/descr.hamlet")
        addScriptRemote "https://code.jquery.com/jquery-3.5.1.slim.min.js"
        addScriptRemote "https://cdn.jsdelivr.net/npm/bootstrap@4.5.3/dist/js/bootstrap.bundle.min.js"



getListaR :: Handler Html
getListaR = do
    produtos <- runDB $ selectList [] [Desc ProdutoValor]
    defaultLayout $ do
        setTitle "Nova tarefa - Lista de produtos"
        addStylesheet (StaticR css_bootstrapmin_css)
        addStylesheet (StaticR css_styles_css)
        toWidgetHead $(luciusFile "templates/listar.lucius")
        $(whamletFile "templates/listar.hamlet") 
        addScriptRemote "https://code.jquery.com/jquery-3.5.1.slim.min.js"
        addScriptRemote "https://cdn.jsdelivr.net/npm/bootstrap@4.5.3/dist/js/bootstrap.bundle.min.js"
   
        


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
    let sql = "SELECT ?? FROM compra WHERE compra.produtoid = ?"
    runDB $ deleteCascade pid
    redirect ListaR