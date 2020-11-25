{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Home where

import Import

--getAdminR :: Handler Html
--getAdminR = do 
--   defaultLayout $ do
--        [whamlet|
--            <h1>
--                Bem vindo admin!     
--        |]


getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    setTitle "Nova tarefa - Cadastre suas tarefas"
    addStylesheet (StaticR css_bootstrapmin_css)
    addStylesheet (StaticR css_styles_css)
    addScriptRemote "https://code.jquery.com/jquery-3.5.1.slim.min.js"
    toWidgetHead [hamlet|
        <script src=@{StaticR js_ola_js}>
    |]
    sess <- lookupSession "_ID"
    [whamlet|
        <h1>
            Gerenciador de Tarefas
        <br>
        <img src=@{StaticR imgs_NovaTarefa_png}><br>

        <ul>
            $maybe sessao <- sess
                <ul class="nav nav-tabs">
                    <li role="presentation" class="active">
                        <a href=@{HomeR}>
                            Home
                    <li role="presentation">
                        <a href=@{ProdutoR}>
                            Lista do supermecado
                    <li role="presentation">
                        <a href=@{ListaR}>
                            Lista de compras
                    <li role="presentation">
                        <a href=@{TarefaR}>
                            Cadastrar nova tarefa
                    <li role="presentation">
                        <a href=@{ListaTaR}>
                            Minhas tarefas
                    <li role="presentation">
                        <form action=@{LogoutR} method=post>
                            <input type="submit" value="Logout">
            $nothing
                <li>
                    <a href=@{UsuarioR}>
                        Cadastrar
                <li>
                    <a href=@{LoginR}>
                        Entrar
                
    |]