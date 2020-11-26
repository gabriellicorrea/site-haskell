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
    addScriptRemote "https://cdn.jsdelivr.net/npm/bootstrap@4.5.3/dist/js/bootstrap.bundle.min.js"
    toWidgetHead [hamlet|
        <script src=@{StaticR js_ola_js}>
    |]
    sess <- lookupSession "_ID"
    [whamlet|
        <h1 class="text-center">
            Gerenciador de Tarefas
        <br>
        <img src=@{StaticR imgs_NovaTarefa_png} class="img-logo"><br>

        
            $maybe sessao <- sess
               <h2>Teste</h2>
            $nothing
                <ul>
                    <li>
                        <a href=@{UsuarioR}>
                            Cadastrar
                    <li>
                        <a href=@{LoginR}>
                            Entrar
                
    |]