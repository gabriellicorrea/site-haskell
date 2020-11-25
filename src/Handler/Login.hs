{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Login where

import Import
import Text.Lucius
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

formLogin :: Form Usuario
formLogin = renderBootstrap3 BootstrapBasicForm $ Usuario
            <$> areq emailField (FieldSettings "Email: " 
                                        Nothing
                                        (Just "h21")
                                        Nothing
                                        [("class", "form-control")]) 
                                        Nothing
            
            <*> areq passwordField  (FieldSettings "Senha: " 
                                        Nothing
                                        (Just "h21")
                                        Nothing
                                        [("class", "form-control")]) 
                                        Nothing

getLoginR :: Handler Html
getLoginR = do
     (formWidget, _) <- generateFormPost formLogin
     mensagem <- getMessage
     defaultLayout $ do
        addStylesheet (StaticR css_bootstrap_css)
        [whamlet|
            <div>

                $maybe msg <- mensagem 
                    ^{msg}
            <form action=@{LoginR} method=post class="form-signin">
                <h2 class="form-signin-heading">Por favor, faça login
                ^{formWidget}
                <input type="submit" class="btn btn-lg btn-primary btn-block" value="Entrar">
        |]


postLoginR :: Handler Html
postLoginR = do
    ((result, _), _) <- runFormPost formLogin 
    case result of
         --FormSuccess (Usuario "root@root.com" "root") -> do
           -- setSession "_ID" "admin"
            --redirect AdminR
         FormSuccess (Usuario email senha) -> do
            usuario <- runDB $ getBy (UniqueEmail email)
            case usuario of 
                 Just (Entity _ (Usuario _ senhaBanco)) -> do 
                    if (senha == senhaBanco) then do
                        setSession "_ID" email
                        redirect HomeR
                    else do
                        setMessage [shamlet|
                            <h1>
                                Senha Invalida
                        |]
                        redirect LoginR
                 Nothing -> do
                    setMessage [shamlet|
                        <h1>
                            Usuario nao encontrado
                    |]
                    redirect LoginR
         _ -> redirect HomeR

postLogoutR :: Handler Html
postLogoutR = do
    deleteSession "_ID"
    redirect HomeR

