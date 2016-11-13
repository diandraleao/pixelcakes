{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module Handler.Contato where

import Foundation
import Yesod
import Data.Text
import Control.Applicative
import Database.Persist.Postgresql

formContato :: Form Contato
formContato = renderDivs $ Contato
    <$> areq textField "Nome do contato"    Nothing
    <*> areq textField "E-mail do contato"  Nothing
    <*> areq textField "Mensagem"           Nothing

getContatoR :: Handler Html
getContatoR = do
            (widget, enctype) <- generateFormPost formContato
            defaultLayout [whamlet|
             <form method=post action=@{ContatoR} enctype=#{enctype}>
                 ^{widget}
                 <input type="submit" value="Cadastrar">
         |]

postContatoR :: Handler Html
postContatoR = do
            ((result, _), _) <- runFormPost formContato
            case result of
                FormSuccess contato -> do
                    alid <- runDB $ insert contato
                    defaultLayout [whamlet|
                        Contato feito com sucesso #{fromSqlKey alid}!
                    |]
                _ -> redirect IndexR

-- SELECT * FROM aluno ORDER BY nome
getListContR :: Handler Html
getListContR = do
            contatos <- runDB $ selectList [] [Asc ContatoNome]
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
                                        <li class="dropdown">
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
                        <h2>Contatos cadastrados</h2>
                        <table class="table">
                            <thead>
                                <tr> 
                                    <th> id  
                                    <th> nome 
                                    <th> e-mail
                                    <th> mensagem
                                    <th> excluir
                            $forall Entity alid contato <- contatos
                                <tr>
                                    <form action=@{DelContatoR alid} method=post> 
                                        <td> #{fromSqlKey  alid}  
                                        <td> #{contatoNome  contato} 
                                        <td> #{contatoEmail  contato} 
                                        <td> #{contatoMensagem    contato}
                                        <td> <input type="submit" value="excluir">
                |]
                
postDelContatoR :: ContatoId -> Handler Html
postDelContatoR alid = do 
                runDB $ delete alid
                redirect ListProdR