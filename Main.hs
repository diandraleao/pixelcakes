{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
import Foundation
import Application () -- for YesodDispatch instance
import Yesod
import Control.Monad.Logger (runStdoutLoggingT)
import Database.Persist.Postgresql

connStr :: ConnectionString
connStr = "dbname=dc7pqe1jb61aio host=ec2-54-243-52-209.compute-1.amazonaws.com user=kuvbwyvyzifgee password=BbsUiNRRyVGTDi53IbjYrP1m4K port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool 
       warp 8080 (App pool)