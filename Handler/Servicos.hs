{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Handler.Servicos where

import Foundation
import Yesod
import Data.Text
import Control.Applicative
import Database.Persist.Postgresql

formDisc :: Form Servicos
formDisc = renderDivs $ Servicos
    <$> areq textField "Serviço oferecido"      Nothing
    <*> areq textField "Descrição do serviço"   Nothing

getDiscR :: Handler Html
getDiscR = do
            (widget, enctype) <- generateFormPost formDisc
            defaultLayout [whamlet|
             <form method=post action=@{DiscR} enctype=#{enctype}>
                 ^{widget}
                 <input type="submit" value="Cadastrar">
         |]

postDiscR :: Handler Html
postDiscR = do
            ((result, _), _) <- runFormPost formDisc
            case result of
                FormSuccess servicos -> do
                    did <- runDB $ insert servicos
                    defaultLayout [whamlet|
                        Serviço cadastrado com sucesso #{fromSqlKey did}!
                    |]
                _ -> redirect HomeR

getListDiscR :: Handler Html
getListDiscR = do
            servicos <- runDB $ selectList [] [Asc ServicosNome]
            defaultLayout $ do
                [whamlet|
                    <table>
                        <tr> 
                            <td> id  
                            <td> nome 
                            <td> descricao
                        $forall Entity did disc <- servicos
                            <tr> 
                                <form action=@{DelDiscR did} method=post> 
                                    <td> #{fromSqlKey  did}  
                                    <td> #{servicosNome  disc}
                                    <td> #{servicosDescricao  disc}
                                    <td> <input type="submit" value="excluir">
                    |]

postDelDiscR :: ServicosId -> Handler Html
postDelDiscR did = do 
                runDB $ delete did
                redirect ListDiscR