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

--    authRoute _ = Just LoginR
    
--  isAutorized HomeR _ = return Autorized
--    isAutorized LoginR _ = return Autorized
--    isAutorized ProdutoR _ = return Autorized
--    isAutorized AdminR _ = return Autorized
--    isAutorized _ _ = isUsuario

    defaultLayout contents = do
        PageContent title headTags bodyTags <- widgetToPageContent contents
        mmsg <- getMessage
        withUrlRenderer [hamlet|
            $doctype 5

            <html>
                <head>
                    <title>#{title}
                    ^{headTags}
                <body>
                    $maybe msg <- mmsg
                        <div #message>#{msg}
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
