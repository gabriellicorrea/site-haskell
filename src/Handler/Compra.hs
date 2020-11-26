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
                    let sql = "SELECT ??,??,?? FROM usuario INNER JOIN compra ON compra.usuarioid = usuario.id INNER JOIN produto ON compra.produtoid = produto.id WHERE usuario.id = ?"
                    produtos <- runDB $ rawSql sql [toPersistValue uid] :: Handler [(Entity Usuario, Entity Compra, Entity Produto)]
                    defaultLayout $ do
                        setTitle "Nova tarefa -Minhas compras"
                        addStylesheet (StaticR css_bootstrapmin_css)
                        addStylesheet (StaticR css_styles_css)
                        [whamlet|
                            <h1 class="text-center mt-5">
                               Minhas compras, logado com:  #{usuarioEmail usuario}
                            <table class="table table-striped mt-4">
                                <thead>
                                    <tr>
                                        <th>
                                            Nome
                                        <th>
                                            Valor
                                <tbody>

                                    $forall (Entity _ _, Entity _ compra, Entity _ produto) <- produtos
                                        <tr>
                                            <td>
                                                #{produtoNome produto}
                                            <td>
                                                R$ #{produtoValor produto * (fromIntegral (compraQtunit compra))}
                                
                        |]
                        addScriptRemote "https://code.jquery.com/jquery-3.5.1.slim.min.js"
                        addScriptRemote "https://cdn.jsdelivr.net/npm/bootstrap@4.5.3/dist/js/bootstrap.bundle.min.js"

postComprarR :: ProdutoId -> Handler Html
postComprarR pid = do
    ((resp,_),_) <- runFormPost formQt
    case resp of 
        FormSuccess qt -> do
            sess <- lookupSession "_ID"
            case sess of    
                Nothing -> redirect HomeR
                Just email -> do
                    usuario <- runDB $ getBy(UniqueEmail email)
                    case usuario of 
                         Nothing -> redirect HomeR
                         Just (Entity uid _) -> do
                              runDB $ insert (Compra pid uid qt)
                              redirect ListComprarR
        _-> redirect HomeR


