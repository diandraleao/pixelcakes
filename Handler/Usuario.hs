{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
module Handler.Usuario where --nome da pasta e nome do arquivo

import Foundation
import Yesod
import Yesod.Static
import Data.Text
import Control.Applicative
import Database.Persist.Postgresql

formUser :: Form Usuario
formUser = renderDivs $ Usuario
    <$> areq emailField    "E-mail: "  Nothing
    <*> areq passwordField "Senha: "   Nothing

getLoginR :: Handler Html
getLoginR = do
    (widget,enctype)<- generateFormPost formUser
    defaultLayout $ do
        setTitle "Adiministrator | Pixel Cakes"
        addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
        addStylesheetRemote "https://fonts.googleapis.com/css?family=Raleway"
        addStylesheet $ StaticR css_main_css
        [whamlet|
            <section id="loginadmin">
                <img src=@{StaticR img_logo_png} alt="" id="imglogin">
                <h1>Adiministrator
                <form action=@{LoginR} method=post enctype=#{enctype}>
                    ^{widget}
                    <input type="submit" value="Logar">
        |]

getUsuarioR :: Handler Html
getUsuarioR = do
    (widget,enctype)<- generateFormPost formUser
    defaultLayout $ do
        [whamlet|
            <form action=@{UsuarioR} method=post enctype=#{enctype}>
                ^{widget}
                <input type="submit" value="Cadastrar">
        |]

postUsuarioR :: Handler Html
postUsuarioR = do
    ((resultado,_),_)<- runFormPost formUser
    case resultado of
        FormSuccess user -> do
            uid <- runDB $ insert user
            defaultLayout [whamlet|
                Usuárix cadastrado com e-mail #{usuarioEmail user}
            |]
        _ -> redirect HomeR

-- ROTA DE AUTENTICACAO
postLoginR :: Handler Html
postLoginR = do
    ((resultado,_),_)<- runFormPost formUser
    case resultado of
        FormSuccess user -> do
            usuario <- runDB $ selectFirst [UsuarioEmail ==. (usuarioEmail user),
                                    UsuarioSenha ==. (usuarioSenha user)] []
            case usuario of
                Nothing -> redirect LoginR
                Just (Entity uid _) -> do
                    setSession "_ID" (pack $ show uid)
                    redirect PerfilR
        _ -> redirect HomeR

getPerfilR :: Handler Html
getPerfilR = do
    userId <- lookupSession "_ID"
    case userId of
        Just str -> do
            usuario <- runDB $ get404 (read (unpack str))
            defaultLayout $ do
                setTitle "Adiministrator | Pixel Cakes"
                addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
                addStylesheetRemote "https://fonts.googleapis.com/css?family=Raleway"
                addStylesheet $ StaticR css_main_css
                [whamlet|
                    <div class="container">
                        <img src=@{StaticR img_logo_png} alt="Logo Pixel Cakes" class="center-block" id="imglogin">
                        <nav class="navbar navbar-default" id="navbar-admin">
                            <div class="container-fluid">
                                <div class="navbar-header">
                                    <button type="button" class="navbar-toggle collapsed" data-toggle="collapse" data-target="#navbar" aria-expanded="false" aria-controls="navbar">
                                        <span class="sr-only">Navegação alternativa
                                        <span class="icon-bar">
                                        <span class="icon-bar">
                                        <span class="icon-bar">
        
                                <div id="navbar" class="navbar-collapse collapse">
                                    <ul class="nav navbar-nav">
                                        <li>
                                            <a href="#">home
                                        <li>
                                            <a href="#">serviços
                                        <li>
                                            <a href="#">produtos
                                        <li>
                                            <a href="#">contatos
                                        <li>
                                            <a href="#">usuários

                        <article id="logado" class="">
                            <h1> Bem-vindo!
                                <small> Logado com #{usuarioEmail usuario}!
                |]
        Nothing -> defaultLayout [whamlet|
                    <h1> Não Logadoooo!!!! 
                |]

postLogoutR :: Handler Html
postLogoutR = do
    deleteSession "_ID"
    redirect HomeR