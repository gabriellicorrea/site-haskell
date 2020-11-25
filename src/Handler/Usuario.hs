{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Usuario where

import Import
import Text.Lucius
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

formUsuario :: Form (Usuario, Text)
formUsuario = renderBootstrap3 BootstrapBasicForm $ (,) 
    <$> (Usuario 
            <$> areq emailField (FieldSettings "Email: " 
                                        Nothing
                                        (Just "h21")
                                        Nothing
                                        [("class", "form-control")]) 
                                        Nothing
            <*> areq passwordField (FieldSettings "Senha: " 
                                        Nothing
                                        (Just "h21")
                                        Nothing
                                        [("class", "form-control")]) 
                                        Nothing
        )
    <*> areq passwordField (FieldSettings "Confirmar senha: " 
                                        Nothing
                                        (Just "h21")
                                        Nothing
                                        [("class", "form-control")]) 
                                        Nothing

getUsuarioR :: Handler Html
getUsuarioR = do
     (formWidget, _) <- generateFormPost formUsuario
     mensagem <- getMessage
     defaultLayout $ do
        setTitle "Nova tarefa - cadastro"
        addStylesheet (StaticR css_bootstrapmin_css)
        addStylesheet (StaticR css_styles_css)
        [whamlet|
            <div>
                $maybe msg <- mensagem 
                    ^{msg}
            <img src=@{StaticR imgs_NovaTarefa_png} class="img-logo">      
            <form action=@{UsuarioR} method=post class="form-signin">
                ^{formWidget}
                <input type="submit" class="btn btn-lg btn-primary btn-block" value="Cadastrar">
            
            <p class="text-center text-muted">
                ou
            <a href=@{LoginR} class="text-center font-weight-bold w-100 form-sigin-link">
                Entrar
        |]


postUsuarioR :: Handler Html
postUsuarioR = do
    ((result, _), _) <- runFormPost formUsuario 
    case result of
         FormSuccess (Usuario email senha, conf) -> do
            if (senha == conf) then do
                runDB $ insert400 (Usuario email senha)
                redirect HomeR
            else do
                setMessage [shamlet|
                    <h1>
                        Senhas nao conferem
                |]
                redirect UsuarioR
         _ -> redirect HomeR
