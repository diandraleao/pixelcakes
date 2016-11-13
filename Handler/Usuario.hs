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
        setTitle "Administrator | Pixel Cakes"
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
                setTitle "Administrator | Pixel Cakes"
                addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
                addStylesheetRemote "https://fonts.googleapis.com/css?family=Raleway"
                addStylesheet $ StaticR css_main_css
                addScriptRemote "https://code.jquery.com/jquery-3.1.1.min.js"
                addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
                addScript $ StaticR js_main_js
                [whamlet|
                    ^{nav}
                        <div class="container">
                            <h2>Cadastrar usuário</h2>
                            <form action=@{UsuarioR} method=post enctype=#{enctype}>
                                ^{widget}
                                <input type="submit" value="Cadastrar" class="btn btn-default">
                    ^{footer}
            |]

postUsuarioR :: Handler Html
postUsuarioR = do
    ((resultado,_),_)<- runFormPost formUser
    case resultado of
        FormSuccess user -> do
            uid <- runDB $ insert user
            defaultLayout $ do
                setTitle "Administrator | Pixel Cakes"
                addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
                addStylesheetRemote "https://fonts.googleapis.com/css?family=Raleway"
                addStylesheet $ StaticR css_main_css
                addScriptRemote "https://code.jquery.com/jquery-3.1.1.min.js"
                addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
                addScript $ StaticR js_main_js
                [whamlet|
                    ^{nav}
                        <div class="container">
                            <h2 class="alert alert-success text-center">Usuário cadastrado com sucesso!
                        <p class="text-uppercase text-center">E-mail utilizado: #{usuarioEmail user}
                    ^{footer}
            |]
        _ -> redirect IndexR

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
        _ -> redirect IndexR

getPerfilR :: Handler Html
getPerfilR = do
    userId <- lookupSession "_ID"
    case userId of
        Just str -> do
            usuario <- runDB $ get404 (read (unpack str))
            defaultLayout $ do
                setTitle "Administrator | Pixel Cakes"
                addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
                addStylesheetRemote "https://fonts.googleapis.com/css?family=Raleway"
                addStylesheet $ StaticR css_main_css
                addScriptRemote "https://code.jquery.com/jquery-3.1.1.min.js"
                addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
                addScript $ StaticR js_main_js
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
                                            <li class="divider">
                                                <a href=@{PerfilR}>home
                                            <li class="dropdown">
                                                <a href="#" class="dropdown-toggle" data-toggle="dropdown" role="button" aria-haspopup="true" aria-expanded="false">
                                                    serviços <span class="caret">
                                                <ul class="dropdown-menu">
                                                    <li>
                                                        <a href=@{ServicoR}>
                                                            <span class="glyphicon glyphicon-plus"></span>
                                                            adicionar
                                                    
                                                    <li role="separator" class="divider"></li>
                                                    <li>
                                                        <a href=@{ListServR}>
                                                            <span class="glyphicon glyphicon-th-list"></span>
                                                            ver todos
                                            <li class="dropdown">
                                                <a href="#" class="dropdown-toggle" data-toggle="dropdown" role="button" aria-haspopup="true" aria-expanded="false">
                                                    produtos <span class="caret">
                                                <ul class="dropdown-menu">
                                                    <li>
                                                        <a href=@{ProdutoR}>
                                                            <span class="glyphicon glyphicon-plus"></span>
                                                            adicionar
                                                    
                                                    <li role="separator" class="divider"></li>
                                                    <li>
                                                        <a href=@{ListProdR}>
                                                            <span class="glyphicon glyphicon-th-list"></span>
                                                            ver todos
                                            <li>
                                                <a href=@{ListContR}>contatos
                                            <li class="dropdown active">
                                                <a href="#" class="dropdown-toggle" data-toggle="dropdown" role="button" aria-haspopup="true" aria-expanded="false">
                                                    usuários <span class="caret">
                                                <ul class="dropdown-menu">
                                                    <li>
                                                        <a href=@{UsuarioR}>
                                                            <span class="glyphicon glyphicon-plus"></span>
                                                            adicionar
                                                    
                                                    <li role="separator"></li>
                                                    <li>
                                                        <a href=@{ListUsuarioR}>
                                                            <span class="glyphicon glyphicon-th-list"></span>
                                                            ver todos
                                        <ul class="nav navbar-nav navbar-right">
                                            <li>
                                                <form action=@{LogoutR} method=post>
                                                    <input type="submit" value="sair" class="btn-sair">
                        <h1> Bem-vindo!
                            <small> Logado com #{usuarioEmail usuario}
                    ^{footer}
                |]
        Nothing -> defaultLayout [whamlet|
                    <h1> Não logado! 
                |]

postLogoutR :: Handler Html
postLogoutR = do
    deleteSession "_ID"
    redirect IndexR

getListUsuarioR :: Handler Html
getListUsuarioR = do
            usuarios <- runDB $ selectList [] [Asc UsuarioEmail]
            defaultLayout $ do
                setTitle "Administrator | Pixel Cakes"
                addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
                addStylesheetRemote "https://fonts.googleapis.com/css?family=Raleway"
                addStylesheet $ StaticR css_main_css
                addScriptRemote "https://code.jquery.com/jquery-3.1.1.min.js"
                addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
                addScript $ StaticR js_main_js
                [whamlet|
                    ^{nav}
                        <div class="container">
                            <h2>Usuários cadastrados</h2>
                            <table class="table">
                                <thead>
                                    <tr> 
                                        <th> id  
                                        <th> e-mail 
                                        <th> senha
                                        <th> excluir
                                $forall Entity alid usuario <- usuarios
                                    <tr>
                                        <form action=@{DelUsuarioR alid} method=post> 
                                            <td> #{fromSqlKey  alid}  
                                            <td> #{usuarioEmail  usuario} 
                                            <td> #{usuarioSenha  usuario}
                                            <td> <input type="submit" value="excluir">
                    ^{footer}
                |]

postDelUsuarioR :: UsuarioId -> Handler Html
postDelUsuarioR did = do 
                runDB $ delete did
                redirect ListUsuarioR

footer :: Widget
footer = [whamlet|
                <footer>
                    <div class="container">
                        <hr>
                        <p class="text-center"><span class="glyphicon glyphicon-star-empty"></span> Desenvolvido por Diandra, Fabiana e Rosilene. Recuse imitações <span class="glyphicon glyphicon-star-empty"></span>
                    
              |]

nav :: Widget
nav = [whamlet|
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
                                    <a href=@{PerfilR}>home
                                <li class="dropdown">
                                    <a href="#" class="dropdown-toggle" data-toggle="dropdown" role="button" aria-haspopup="true" aria-expanded="false">
                                        serviços <span class="caret">
                                    <ul class="dropdown-menu">
                                        <li>
                                            <a href=@{ServicoR}>
                                                <span class="glyphicon glyphicon-plus"></span>
                                                adicionar
                                        
                                        <li role="separator" class="divider"></li>
                                        <li>
                                            <a href=@{ListServR}>
                                                <span class="glyphicon glyphicon-th-list"></span>
                                                ver todos
                                <li class="dropdown">
                                    <a href="#" class="dropdown-toggle" data-toggle="dropdown" role="button" aria-haspopup="true" aria-expanded="false">
                                        produtos <span class="caret">
                                    <ul class="dropdown-menu">
                                        <li>
                                            <a href=@{ProdutoR}>
                                                <span class="glyphicon glyphicon-plus"></span>
                                                adicionar
                                        
                                        <li role="separator" class="divider"></li>
                                        <li>
                                            <a href=@{ListProdR}>
                                                <span class="glyphicon glyphicon-th-list"></span>
                                                ver todos
                                <li>
                                    <a href=@{ListContR}>contatos
                                <li class="dropdown active">
                                    <a href="#" class="dropdown-toggle" data-toggle="dropdown" role="button" aria-haspopup="true" aria-expanded="false">
                                        usuários <span class="caret">
                                    <ul class="dropdown-menu">
                                        <li>
                                            <a href=@{UsuarioR}>
                                                <span class="glyphicon glyphicon-plus"></span>
                                                adicionar
                                        
                                        <li role="separator" class="divider"></li>
                                        <li>
                                            <a href=@{ListUsuarioR}>
                                                <span class="glyphicon glyphicon-th-list"></span>
                                                ver todos
                            <ul class="nav navbar-nav navbar-right">
                                <li>
                                    <form action=@{LogoutR} method=post>
                                        <input type="submit" value="sair" class="btn-sair">
    |]