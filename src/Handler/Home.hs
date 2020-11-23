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
    toWidgetHead [hamlet|
        <script src=@{StaticR js_ola_js}>
    |]
    sess <- lookupSession "_ID"
    [whamlet|
        <h1>
            Gerenciador de Tarefas
        <br>
        <img src=@{StaticR imgs_tarefas_jpg}><br>

        <ul>
            $maybe sessao <- sess
                <li>
                    <a href=@{ListaR}>
                        LISTAGEM DE TAREFAS
                <li>
                    <form action=@{LogoutR} method=post>
                        <input type="submit" value="Logout">
            $nothing
                <li>
                    <a href=@{LoginR}>
                        Entrar
    |]