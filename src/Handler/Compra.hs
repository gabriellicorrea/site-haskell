{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Compra where

import Import
import Tools
import Database.Persist.Sql

getListComprarR :: Handler Html
getListComprarR = do
    sess <- lookupSession "_ID"
    case sess of
        Nothing -> redirect HomeR
        Just email -> do
            usu <- runDB $ getBy (UniqueEmail email)
            case usu of 
                 Nothing -> redirect HomeR
                 Just (Entity uid usuario) -> do    
                     let sql = "SELECT ??,??,??" FROM usuario \ 
                        \ INNER JOIN compra ON compra.usuarioid = usuario.id \
                        \ INNER JOIN produto ON compra.produtoid = produto.id \
                        \ WHERE usuario.id = ?"
                      produtos <- runDB $ rawSql sql [toPersistValue uid] :: 
                      Handler [(Entity Usuario,Entity Compra,Entity Produto)]
                      defaultLayout $ do
                        [whamlet|
                            <h1>
                                Compras de #{usuarioNome usuario}
                            <ul>
                            $forall {Entity _ _ , Entity _ compra, Entity _ produto} <- produtos
                                    <li>
                                        #{produtoNome produto}: #{produtoValor produto * (formIntegral (compraQtunit compra))}
                        |]

postComprarR :: ProdutoId -> Handler Html
postComprarR pid = do
    ((resp,_),_) <- runFormPost formQt
    case resp of 
        FormSuccess qt -> do
            sess <- lookupSession "_ID"
            case sess of    
                Nothing -> redirect HomeR
                Just  Email -> do
                    usuario <- runDB $ getBy(UniqueEmail email)
                    case usuario of 
                         Nothing -> redirect HomeR
                         Just (Entity uid _) -> do
                              runDB $ insert (Compra uid pid qt)
                              redirect ListComprarR
        _-> redirect HomeR


