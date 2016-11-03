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
userServer config = undefined 
               :<|> postUser config



