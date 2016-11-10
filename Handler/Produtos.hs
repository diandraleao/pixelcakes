{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module Handler.Produtos where

import Foundation
import Yesod
import Data.Text
import Control.Applicative
import Database.Persist.Postgresql

formAluno :: Form Produtos
formAluno = renderDivs $ Produtos
    <$> areq textField "Nome do produto"        Nothing
    <*> areq textField "Descrição do produto"   Nothing
    <*> areq doubleField "Preço"                Nothing

getAlunoR :: Handler Html
getAlunoR = do
            (widget, enctype) <- generateFormPost formAluno
            defaultLayout [whamlet|
             <form method=post action=@{AlunoR} enctype=#{enctype}>
                 ^{widget}
                 <input type="submit" value="Cadastrar">
         |]

postAlunoR :: Handler Html
postAlunoR = do
            ((result, _), _) <- runFormPost formAluno
            case result of
                FormSuccess produto -> do
                    alid <- runDB $ insert produto
                    defaultLayout [whamlet|
                        Cadastradx com sucesso #{fromSqlKey alid}!
                    |]
                _ -> redirect HomeR

-- SELECT * FROM aluno ORDER BY nome
getListAluR :: Handler Html
getListAluR = do
            produtos <- runDB $ selectList [] [Asc ProdutosNome]
            defaultLayout $(whamletFile "templates/tableProduto.hamlet")
                
postDelAlunoR :: ProdutosId -> Handler Html
postDelAlunoR alid = do 
                runDB $ delete alid
                redirect ListAluR

footerzinho :: Widget
footerzinho = [whamlet|
                  <footer>
                      Footer
              |]