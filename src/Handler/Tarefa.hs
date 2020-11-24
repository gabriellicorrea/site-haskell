{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Tarefa where

import Import
import Text.Lucius
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Tools

getTarefaR :: Handler Html
getTarefaR = defaultLayout $ do
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
                        LISTAGEM DE TAREFAS!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                <li>
                    <form action=@{LogoutR} method=post>
                        <input type="submit" value="Logout">
            $nothing
                <li>
                    <a href=@{LoginR}>
                        Entrar
    |]