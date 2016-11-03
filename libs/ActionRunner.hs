{-# LANGUAGE FlexibleContexts      #-}

module ActionRunner where


import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Control
import Configs
import qualified Database.Persist.MySQL as MySQL
import qualified Database.Persist.Sql as SQL
import qualified Database.MySQL.Base.Types as MySQLTypes

-- This function receives a ConfigMySQL and performs an action.
runMySQL :: (MonadIO m, MonadBaseControl IO m) =>
            ConfigMySQL
         -> SQL.SqlPersistM a
         -> m a
runMySQL config action =
  runStdoutLoggingT $ MySQL.withMySQLPool (connectionInfo config)
                                          (poolConnections config)
                                          (\pool -> liftIO $ SQL.runSqlPersistMPool action pool)



