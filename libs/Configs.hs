module Configs where

import System.Environment
import Network.Wai
import Network.Wai.Middleware.RequestLogger
import qualified Database.Persist.MySQL as MySQL
import qualified Database.MySQL.Base.Types as MySQLTypes


data Environment
  = Development | Production | Test
  deriving (Eq, Show)

-- Given an Environment, this function sets a appropriate logger.
setApiLogger :: Environment -> Middleware
setApiLogger Test = id
setApiLogger Development = logStdoutDev
setApiLogger Production = logStdout


-- This function gets the current environment of the API.
getAppEnv :: IO Environment
getAppEnv = do
  env <- lookupEnv "API_ENV"
  case env of
    Just "production" -> return Production
    Just "test" -> return Test
    _ -> return Development


-- This function gets the current port of the API.
getAppPort :: IO Int
getAppPort = do
  port <- lookupEnv "API_PORT"
  case port of
    Nothing -> return 8081
    Just p -> return $ read p


-- This data type is intended to represent the configurations settings
-- to create a MySQL conecction.
data ConfigMySQL = ConfigMySQL {
  connectionInfo :: MySQL.ConnectInfo,
  poolConnections ::Int
  } deriving (Eq, Show)


info :: MySQL.ConnectInfo
info = MySQL.ConnectInfo "localhost"
                         3306
                         "root"
                         "doncellasnoir9."
                         "servant_devel"
                         [MySQLTypes.CharsetName "utf8"]
                         ""
                         Nothing


mysqlDevelConfig = ConfigMySQL info 1 



-- This function gets the configuration of the API
-- according to the evironment.
getConfig :: Environment -> ConfigMySQL
getConfig env
  | env == Development = mysqlDevelConfig
  | env == Production = undefined
  | env == Test = undefined 
