{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module UserAPI where

import Servant.API
import DataTypes

type UserId = Integer

type UserAPI =
       "user" :> Capture "userId" UserId
              :> Get '[JSON] User
  :<|> "users" :> QueryParam "from" Int
               :> QueryParam "size" Int
               :> Get '[JSON] [User]
  :<|> "user" :> ReqBody '[JSON] User
              :> PostCreated '[JSON] UserId

