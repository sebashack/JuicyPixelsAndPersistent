{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}


module Schemas where

import Configs
import ActionRunner
import Data.ByteString (ByteString)
import Data.Time
import qualified Database.Persist.MySQL as MySQL
import qualified Database.Persist.TH as TH
import qualified Data.Text as T
import qualified Database.Persist as Per

TH.share [TH.mkPersist TH.sqlSettings, TH.mkMigrate "userModel"] [TH.persistLowerCase|
User
  profileName      ByteString
  email            ByteString
  name             T.Text
  lastName         T.Text
  birthDate        UTCTime
  UniqueEmail      email
  ProfileName      profileName
  deriving Show
Image
  code             ByteString  
  userId           UserId
  image            ByteString -- mediumblob
  ImgCode          code
  deriving Show
|]
  

printDevelUser :: IO ()
printDevelUser = runMySQL mysqlDevelConfig $ MySQL.printMigration userModel


