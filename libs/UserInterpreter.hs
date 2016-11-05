{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UserInterpreter (userServer) where

import Servant
import UserAPI
import Configs
import ActionRunner
import Control.Monad.Except
import Control.Monad.IO.Class
import qualified DataTypes as DT
import qualified Schemas as US
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Database.Persist as Per
import qualified Database.Persist.MySQL as MySQL
import qualified Data.Time as TM



getUser :: ConfigMySQL
        -> Integer
        -> ExceptT ServantErr IO DT.User
getUser config userId = do
  eitherUser <- liftIO $ runMySQL config  $ do
    let userKey = US.UserKey $ MySQL.SqlBackendKey (fromInteger userId)
    maybeUser <- Per.get userKey
    case maybeUser of
      Nothing -> return $ Left "User Not Found"
      Just (US.User profName email name lName birth) -> 
        return $ Right $ DT.User (TE.decodeUtf8 profName)
                                 name
                                 lName
                                 birth
                                 (TE.decodeUtf8 email)
  either (\error -> throwError err404 { errBody = error }) (\v -> return v) eitherUser



getUsers :: ConfigMySQL
         -> Maybe Int
         -> Maybe Int
         -> ExceptT ServantErr IO [DT.User]
getUsers config from size = do
  users <- liftIO $ runMySQL config  $ do
    let userKey = US.UserKey $ MySQL.SqlBackendKey 0
    repoUsers <- Per.selectList [US.UserId Per.>. userKey] [Per.OffsetBy from', Per.LimitTo size']
    return $ fmap entityToUser repoUsers
  return users  
  where
    from' = maybe 0 (\v -> v) from
    size' = maybe 1 (\v -> v) size
    entityToUser (Per.Entity userKey repoUser) =
      let (US.User profName email name lName birth) = repoUser
      in DT.User (TE.decodeUtf8 profName) name lName birth (TE.decodeUtf8 email)



postUser :: ConfigMySQL
         -> DT.User
         -> ExceptT ServantErr IO Integer
postUser config uData = do
  userId <- liftIO $ runMySQL config  $ do
    let (DT.User profName name lastName birth email) = uData
        user = US.User (TE.encodeUtf8 profName) (TE.encodeUtf8 email) name lastName birth
    userKey <- Per.insert user  
    return $ toInteger $ MySQL.unSqlBackendKey (US.unUserKey userKey)    
  return userId


userServer :: ConfigMySQL -> Server UserAPI
userServer config = getUser config
               :<|> getUsers config
               :<|> postUser config



