{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ViewPatterns         #-}
{-# LANGUAGE QuasiQuotes       #-}

module Application where

import Foundation
import Yesod

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
                <li> <a href=@{AlunoR}>Cadastro de produto
                <li> <a href=@{ListAluR}>Listagem de produto
                <li> <a href=@{DiscR}>Cadastro de serviços
                <li> <a href=@{ListDiscR}>Listagem de serviços
                <li> <a href=@{ContatoR}>Cadastro de contato
                <li> <a href=@{ListContR}>Listagem de contatos
                $maybe _ <- sess
                    <li> 
                        <form action=@{LogoutR} method=post>
                            <input type="submit" value="Logout">
                $nothing
                    <li> <a href=@{LoginR}>Login
        |]