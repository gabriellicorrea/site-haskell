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
   
    toWidgetHead [hamlet|
        <script src=@{StaticR js_ola_js}>
    |]
    sess <- lookupSession "_ID"
    [whamlet|
    <div class="position-relative overflow-hidden p-3 p-md-5 m-md-3 text-center bg-light">
        <div class="col-md-5 p-lg-5 mx-auto my-5">
            <img src=@{StaticR imgs_NovaTarefa_png} class="img-logo-inicio"><br>
            <h1 class="display-4 font-weight-normal" hidden>Gerenciador de tarefas
            <p class="lead font-weight-normal">Planeje sua vida, gerencie suas tarefas de forma mais rápida e prática!
        <br>
      
        
            $maybe sessao <- sess
                <a class="btn btn-outline-secondary" href=@{TarefaR}>Cadastre uma tarefa</a>
            $nothing
                <a href=@{UsuarioR} class="btn btn-outline-secondary">
                            Cadastrar
                <p>
                    ou 
                <a href=@{LoginR} class="btn btn-outline-secondary">
                            Entrar
                
    |]
    addScriptRemote "https://code.jquery.com/jquery-3.5.1.slim.min.js"
    addScriptRemote "https://cdn.jsdelivr.net/npm/bootstrap@4.5.3/dist/js/bootstrap.bundle.min.js"