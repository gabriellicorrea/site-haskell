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
import Database.Persist.Sql

formTarefa :: Maybe Tarefa -> Form (Text,Text) 
formTarefa mr = renderDivs $ (,)
    <$> areq textField (FieldSettings "Nome: " 
                                        Nothing
                                        (Just "h21")
                                        Nothing
                                        [("class", "form-control")])  
                                        (fmap tarefaNome mr)
    <*> areq textField (FieldSettings "Descrição: " 
                                        Nothing
                                        (Just "h21")
                                        Nothing
                                        [("class", "form-control")])  
                                        (fmap tarefaDescricao mr)


auxTarefaR :: Route App -> Maybe Tarefa -> Handler Html
auxTarefaR rt mp = do
     (formWidget, _) <- generateFormPost (formTarefa mp)
     defaultLayout $ do
        setTitle "Nova tarefa - Cadastrar tarefa"
        addStylesheet (StaticR css_bootstrapmin_css)
        addStylesheet (StaticR css_styles_css)
        toWidgetHead $(luciusFile "templates/descr.lucius")
        [whamlet|
            <h1 class="text-center mt-4">Salvar tarefa</h1>
            <form action=@{rt} method=post class="mt-5">
                ^{formWidget}
                <input type="submit" class="btn btn-lg btn-primary btn-block" value="Salvar">
        |]
        addScriptRemote "https://code.jquery.com/jquery-3.5.1.slim.min.js"
        addScriptRemote "https://cdn.jsdelivr.net/npm/bootstrap@4.5.3/dist/js/bootstrap.bundle.min.js"

   
getTarefaR :: Handler Html
getTarefaR = auxTarefaR TarefaR Nothing

postTarefaR :: Handler Html
postTarefaR = do
    ((result, _), _) <- runFormPost (formTarefa Nothing)
    case result of
         FormSuccess (nome, descricao) -> do
            sess <- lookupSession "_ID"
            case sess of    
                Nothing -> redirect HomeR
                Just email -> do
                    usuario <- runDB $ getBy(UniqueEmail email)
                    case usuario of 
                         Nothing -> redirect HomeR
                         Just (Entity uid _) -> do
                              _ <- runDB $ insertEntity (Tarefa uid nome descricao)
                              redirect ListaTaR
         _ -> redirect HomeR

getDesctaR :: TarefaId -> Handler Html
getDesctaR pid = do
    tarefa <- runDB $ get404 pid
    defaultLayout $ do
        setTitle "Nova tarefa - Descrição tarefa"
        addStylesheet (StaticR css_bootstrapmin_css)
        addStylesheet (StaticR css_styles_css)
        $(whamletFile "templates/desctar.hamlet")
        addScriptRemote "https://code.jquery.com/jquery-3.5.1.slim.min.js"
        addScriptRemote "https://cdn.jsdelivr.net/npm/bootstrap@4.5.3/dist/js/bootstrap.bundle.min.js"
        
getListaTaR :: Handler Html
getListaTaR = do
    sess <- lookupSession "_ID"
    case sess of
        Nothing -> redirect HomeR
        Just email -> do
            usu <- runDB $ getBy (UniqueEmail email)
            case usu of 
                Nothing -> redirect HomeR
                Just (Entity uid usuario) -> do    
                    let sql = "SELECT ??,?? FROM usuario INNER JOIN tarefa ON tarefa.usuarioid = usuario.id WHERE usuario.id = ?"
                    tarefas <- runDB $ rawSql sql [toPersistValue uid] :: Handler [(Entity Usuario, Entity Tarefa)]
                    defaultLayout $ do
                        setTitle "Nova tarefa - Minhas tarefas"
                        addStylesheet (StaticR css_bootstrapmin_css)
                        addStylesheet (StaticR css_styles_css)
                        [whamlet|
                            <h1 class="text-center mt-5">
                               Minhas tarefas
                            <table class="table table-striped mt-4">
                                <thead>
                                    <tr>
                                        <th>
                                            Nome
                                        <th>
                                            Descrição
                                        <th>
                                        <th>
                                <tbody>

                                    $forall (Entity _ _, Entity pid tarefa) <- tarefas
                                        <tr>
                                            <td href=@{DesctaR pid}>
                                                #{tarefaNome tarefa}
                                            <td>
                                                #{tarefaDescricao tarefa}
                                            <td>
                                                <a href=@{UpdTarR pid} class="btn btn-secondary">
                                                    <i class="fas fa-pen">
                                                    Editar
                                            <td>
                                                <form action=@{DelTarR pid} method=post>
                                                    <button type="submit" class="btn btn-danger">
                                                        <i class="fas fa-trash-alt">
                                                        Deletar
                        |]
                        addScriptRemote "https://code.jquery.com/jquery-3.5.1.slim.min.js"
                        addScriptRemote "https://cdn.jsdelivr.net/npm/bootstrap@4.5.3/dist/js/bootstrap.bundle.min.js"

getUpdTarR :: TarefaId -> Handler Html
getUpdTarR pid = do
    antigo <- runDB $ get404 pid
    auxTarefaR (UpdTarR pid) (Just antigo)

postUpdTarR :: TarefaId -> Handler Html
postUpdTarR pid = do
    ((result, _), _) <- runFormPost (formTarefa Nothing)
    case result of
        FormSuccess (nome, descricao) -> do
            sess <- lookupSession "_ID"
            case sess of    
                Nothing -> redirect HomeR
                Just email -> do
                    usuario <- runDB $ getBy(UniqueEmail email)
                    case usuario of 
                        Nothing -> redirect HomeR
                        Just (Entity uid _) -> do  
                            let novaTar = Tarefa uid nome descricao                  
                            _ <- runDB (replace pid novaTar)
                            redirect ListaTaR
        _ -> redirect HomeR

postDelTarR :: TarefaId -> Handler Html
postDelTarR pid = do
    runDB $ delete pid
    redirect ListaTaR