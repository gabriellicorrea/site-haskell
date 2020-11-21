{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    toWidgetHead [hamlet|
        <script src=@{StaticR js_ola_js}>
    |]
    [whamlet|
            <h1>
                Gerenciador de Tarefas
            <br>
            <img src=@{StaticR imgs_tarefas_jpg}><br>

            <a href=@{ProdutoR}>
                CADASTRO DE TAREFAS!!
    |]