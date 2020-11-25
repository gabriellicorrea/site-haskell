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
    <$> areq textField  "Nome: " (fmap tarefaNome mp)
    <*> areq textField "Descricao: " (fmap tarefaDescricao mp)


auxTarefaR :: Route App -> Maybe Tarefa -> Handler Html
auxTarefaR rt mp = do
     (formWidget, _) <- generateFormPost (formTarefa mp)
     defaultLayout $ do
        addStylesheet (StaticR css_bootstrap_css)
        toWidgetHead $(luciusFile "templates/descr.lucius")
        [whamlet|
            <form action=@{rt} method=post>
                ^{formWidget}
                <input type="submit"  value="OK">
        |]

   
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
            defaultLayout [whamlet|
                    <h1>tarefa salva galera!aaaaaaaaaaaa  
            |]
         _ -> redirect HomeR

getDesctaR :: TarefaId -> Handler Html
getDesctaR pid = do
    tarefa <- runDB $ get404 pid
    defaultLayout $ do
        $(whamletFile "templates/desctar.hamlet")
        
getListaTaR :: Handler Html
getListaTaR = do
    tarefas <- runDB $ selectList [] [Desc TarefaDescricao]
    defaultLayout $ do
        $(whamletFile "templates/listaTar.hamlet") 


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