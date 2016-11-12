{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ViewPatterns         #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeFamilies      #-}

module Application where

import Foundation
import Yesod
import Yesod.Static

import Handler.Servicos
import Handler.Produtos
import Handler.Usuario
import Handler.Contato
------------------
mkYesodDispatch "App" resourcesApp

getHomeR :: Handler Html
getHomeR = do
    sess <- lookupSession "_ID"
    defaultLayout $ do
        toWidget [lucius|
            ul li {
                display: inline;
            }
            a {
                color: blue;
            }
        |]
        [whamlet|
            <h1> Meu primeiro site em Haskell!
            <ul>
                <li> <a href=@{ProdutoR}>Cadastro de produto
                <li> <a href=@{ListProdR}>Listagem de produto
                <li> <a href=@{ServicoR}>Cadastro de serviços
                <li> <a href=@{ListServR}>Listagem de serviços
                <li> <a href=@{ContatoR}>Cadastro de contato
                <li> <a href=@{ListContR}>Listagem de contatos
                $maybe _ <- sess
                    <li> 
                        <form action=@{LogoutR} method=post>
                            <input type="submit" value="Logout">
                $nothing
                    <li> <a href=@{LoginR}>Login
        |]
        
getIndexR :: Handler Html
getIndexR = defaultLayout $ do
    setTitle "Pixel Cakes"
    toWidgetHead[hamlet|
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <meta http-equiv="X-UA-Compatible" content="ie=edge">
    |]
    addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
    addStylesheetRemote "https://fonts.googleapis.com/css?family=Raleway"
    addStylesheet $ StaticR css_main_css
    addScriptRemote "https://code.jquery.com/jquery-3.1.1.min.js"
    addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
    addScript $ StaticR js_main_js
    --addStylesheet $ StaticR css_bootstrap_min_css
    $(whamletFile "templates/intro.hamlet")
    $(whamletFile "templates/nav.hamlet")
    $(whamletFile "templates/about.hamlet")
    $(whamletFile "templates/parallax.hamlet")
    $(whamletFile "templates/services.hamlet")
    $(whamletFile "templates/gallery.hamlet")
    $(whamletFile "templates/price.hamlet")