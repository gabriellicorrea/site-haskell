{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe

data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static 
    , appConnPool    :: ConnectionPool 
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

mkYesodData "App" $(parseRoutesFile "config/routes.yesodroutes")

instance Yesod App where
    makeLogger = return . appLogger
    defaultLayout contents = do
        PageContent title headTags bodyTags <- widgetToPageContent contents
        mmsg <- getMessage
        sess <- lookupSession "_ID"
        withUrlRenderer [hamlet|
            $doctype 5

            <html>
                <head>
                    <meta charset="utf-8">
                    <meta http-equiv="X-UA-Compatible" content="IE=edge">
                    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">
                    <meta name="description" content="Site desenvolvido para a matéria de Tópicos Especiais">
                    <meta name="author" content="Bianca D., Eliude A., Gabrielli C.">

                    <title>#{title}
                    <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.1/css/all.min.css" integrity="sha512-+4zCK9k+qNFUR5X+cKL9EIR+ZOhtIloNl9GIKS57V1MyNsYpYcUrUeQc9vNfzsWfV28IaLL3i96P9sdNyeRssA==" crossorigin="anonymous" />
                    <script src="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.1/js/fontawesome.min.js" integrity="sha512-kI12xOdWTh/nL2vIx5Yf3z/kJSmY+nvdTXP2ARhepM/YGcmo/lmRGRttI3Da8FXLDw0Y9hRAyZ5JFO3NrCvvXA==" crossorigin="anonymous"></script>
                    ^{headTags}
                    
                <body>
                    $maybe msg <- mmsg
                        <div #message>#{msg}
                    $maybe sessao <- sess
                        <nav class="navbar navbar-expand-md navbar-light bg-light">
                                    <a class="navbar-brand mr-5" href=@{HomeR}>Tarefas
                                    <button class="navbar-toggler" type="button" data-toggle="collapse" data-target="#navbarsExample04" aria-controls="navbarsExample04" aria-expanded="false" aria-label="Toggle navigation">
                                        <span class="navbar-toggler-icon">  

                                    <div class="collapse navbar-collapse" id="navbarsExample04">
                                        <ul class="navbar-nav mr-auto">
                                            <li class="nav-item active mr-3">
                                                <a class="nav-link" href=@{HomeR}>
                                                    Home 
                                            
                                            <li class="nav-item dropdown mr-3" >
                                                <a class="nav-link dropdown-toggle" href="#" id="dropdown04" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">
                                                    Supermercado
                                                <div class="dropdown-menu" aria-labelledby="dropdown04">
                                                    <a class="dropdown-item" href=@{ProdutoR}>
                                                        Adicionar produto
                                                    <a class="dropdown-item" href=@{ListaR}>
                                                        Lista de produtos
                                                    <a class="dropdown-item" href=@{ListComprarR}>
                                                        Meu carrinho


                                            <li class="nav-item dropdown mr-3">
                                                <a class="nav-link dropdown-toggle" href="#" id="dropdown04" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">
                                                    Tarefas
                                                <div class="dropdown-menu" aria-labelledby="dropdown04">
                                                    <a class="dropdown-item" href=@{TarefaR}>
                                                        Adicionar tarefa
                                                    <a class="dropdown-item" href=@{ListaTaR}>
                                                        Minhas tarefas
                                                       
                                            <form action=@{LogoutR} method=post class="form-inline my-2 my-md-0 ml-md-4 justify-content-end">
                                                <button type="submit" class="btn btn-danger">
                                                    <i class="fas fa-sign-out-alt">
                                                    Logout 

                    <div class="container">
                       
                        ^{bodyTags}
        |]

isAdmin :: Handler AuthResult
isAdmin = do 
    sess <- lookupSession "_ID"
    case sess of 
        Nothing -> return AuthenticationRequired
        Just "admin" -> return Authorized
        Just _ -> return $ Unauthorized "VC EH USUARIO COMUM"

isUsuario :: Handler AuthResult
isUsuario = do 
    sess <- lookupSession "_ID"
    case sess of 
        Nothing -> return AuthenticationRequired
        Just _ -> return Authorized

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

type Form a = Html -> MForm Handler (FormResult a, Widget)
        
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance HasHttpManager App where
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger
