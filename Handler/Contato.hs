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
                _ -> redirect HomeR

-- SELECT * FROM aluno ORDER BY nome
getListContR :: Handler Html
getListContR = do
            contatos <- runDB $ selectList [] [Asc ContatoNome]
            defaultLayout $ do
                [whamlet|
                   <table>
                        <tr> 
                            <td> id  
                            <td> nome 
                            <td> email
                            <td> mensagem
                        $forall Entity did disc <- contatos
                            <tr> 
                                <form action=@{DelContatoR did} method=post> 
                                    <td> #{fromSqlKey  did}  
                                    <td> #{contatoNome  disc}
                                    <td> #{contatoEmail  disc}
                                    <td> #{contatoMensagem  disc}
                                    <td> <input type="submit" value="excluir">
                    |]

                
postDelContatoR :: ContatoId -> Handler Html
postDelContatoR alid = do 
                runDB $ delete alid
                redirect ListProdR

footerzinho :: Widget
footerzinho = [whamlet|
                  <footer>
                      Footer
              |]