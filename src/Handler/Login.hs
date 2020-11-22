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
            <$> areq emailField "Email: " Nothing
            <*> areq passwordField "Senha: " Nothing
        )
    <*> areq passwordField "Confirmacao: " Nothing

getUsuarioR :: Handler Html
getUsuarioR = do
     (formWidget, _) <- generateFormPost formUsuario
     mensagem <- getMessage
     defaultLayout $ do
        addStylesheet (StaticR css_bootstrap_css)
        [whamlet|
            <div>
                $maybe msg <- mensagem 
                    ^{msg}
            <form action=@{UsuarioR} method=post>
                ^{formWidget}
                <input type="submit"  value="CADASTRAR">
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
