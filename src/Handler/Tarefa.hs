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

formTarefa :: Maybe Tarefa -> Form Tarefa 
formTarefa mp = renderBootstrap3 BootstrapBasicForm $ Tarefa 
    <$> areq textField (FieldSettings "Nome: " 
                                        Nothing
                                        (Just "h21")
                                        Nothing
                                        [("class", "form-control")])  
                                        (fmap tarefaNome mp)
    <*> areq textField (FieldSettings "Descrição: " 
                                        Nothing
                                        (Just "h21")
                                        Nothing
                                        [("class", "form-control")])  
                                        (fmap tarefaDescricao mp)


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
         FormSuccess tarefa -> do
            pid <- runDB $ insert tarefa
            redirect (DesctaR pid)
            runDB $ insert tarefa
            defaultLayout $ do
                setTitle "Nova tarefa - Sucesso!"
                addStylesheet (StaticR css_bootstrapmin_css)
                addStylesheet (StaticR css_styles_css)
                [whamlet|
                        <h1>Tarefa salva com sucesso! 
                |]
                addScriptRemote "https://code.jquery.com/jquery-3.5.1.slim.min.js"
                addScriptRemote "https://cdn.jsdelivr.net/npm/bootstrap@4.5.3/dist/js/bootstrap.bundle.min.js"
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
    tarefas <- runDB $ selectList [] [Desc TarefaDescricao]
    defaultLayout $ do
        setTitle "Nova tarefa - Cadastrar tarefa"
        addStylesheet (StaticR css_bootstrapmin_css)
        addStylesheet (StaticR css_styles_css)
        $(whamletFile "templates/listaTar.hamlet") 
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
         FormSuccess tarefa -> do
            pid <- runDB $ replace pid tarefa
            redirect ListaTaR 
         _ -> redirect HomeR

postDelTarR :: TarefaId -> Handler Html
postDelTarR pid = do
    runDB $ delete pid
    redirect ListaTaR