{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Categoria where

import Import
import Text.Lucius

import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

formCategoria :: Maybe Categoria -> Form Categoria 
formCategoria mp = renderBootstrap3 BootstrapBasicForm $ Categoria 
    <$> areq textField  "Nome categoria: " (fmap categoriaNome mp)
    <*> areq textField "Descricao categoria: " (fmap categoriaDescricao mp)


auxCategoriaR :: Route App -> Maybe Categoria -> Handler Html
auxCategoriaR rt mp = do
     (formWidget, _) <- generateFormPost (formCategoria mp)
     defaultLayout $ do
        addStylesheet (StaticR css_bootstrap_css)
        toWidgetHead $(luciusFile "templates/descr.lucius")
        [whamlet|
            <form action=@{rt} method=post>
                ^{formWidget}
                <input type="submit"  value="Salvar">
        |]

   
getCategoriaR :: Handler Html
getCategoriaR = auxCategoriaR CategoriaR Nothing

postCategoriaR :: Handler Html
postCategoriaR = do
    ((result, _), _) <- runFormPost (formCategoria Nothing)
    case result of
         FormSuccess categoria -> do
            pid <- runDB $ insert categoria
            redirect (DescCatR pid)
            runDB $ insert categoria
            defaultLayout [whamlet|
                    <h1>tarefa salva galera!aaaaaaaaaaaa  
            |]
         _ -> redirect HomeR

getDescCatR :: CategoriaId -> Handler Html
getDescCatR pid = do
    categoria <- runDB $ get404 pid
    defaultLayout $ do
        $(whamletFile "templates/descCatr.hamlet")
        
getListaCatR :: Handler Html
getListaCatR = do
    categorias <- runDB $ selectList [] [Desc CategoriaDescricao]
    defaultLayout $ do
        $(whamletFile "templates/listaCatr.hamlet") 


getUpdCatR :: CategoriaId -> Handler Html
getUpdCatR pid = do
    antigo <- runDB $ get404 pid
    auxCategoriaR (UpdCatR pid) (Just antigo)

postUpdCatR :: CategoriaId -> Handler Html
postUpdCatR pid = do
    ((result, _), _) <- runFormPost (formCategoria Nothing)
    case result of
         FormSuccess categoria -> do
            pid <- runDB $ replace pid categoria
            redirect ListaCatR 
         _ -> redirect HomeR

postDelCatR :: CategoriaId -> Handler Html
postDelCatR pid = do
    runDB $ delete pid
    redirect ListaCatR