{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Tarefa where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

formTarefa :: Form Tarefa 
formTarefa = renderBootstrap3 BootstrapBasicForm $ Tarefa
    <$> areq textField "Nome: " Nothing
    <*> areq textField "Descricao: " Nothing

   
getTarefaR :: Handler Html
getTarefaR = do
     (formWidget, _) <- generateFormPost formTarefa
     defaultLayout $ do
        [whamlet|
            <form action=@{TarefaR} method=post>
                ^{formWidget}
                <input type="submit"  value="OK">
        |]


postTarefaR :: Handler Html
postTarefaR = do
    ((result, _), _) <- runFormPost formTarefa
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
        [whamlet|
            <ul>
                <li> Nome: #{tarefaNome tarefa}
                <li> Descricao: #{tarefaDescricao tarefa}
        |]