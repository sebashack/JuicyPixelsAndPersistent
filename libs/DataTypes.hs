{-# LANGUAGE DeriveGeneric #-}

module DataTypes where

import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import qualified Data.Time as TM

data User = User {
  profName :: T.Text,
  name :: T.Text,
  lastName :: T.Text,
  birthDate :: TM.UTCTime,
  email :: T.Text
  } deriving (Show, Generic)

instance FromJSON User
instance ToJSON User

instance Eq User where
  (User pN1 _ _ _ _) == (User pN2 _ _ _ _) = pN1 == pN2



data UserImg = UserImg {
  imgId :: T.Text,
  userId :: Integer
  } deriving (Show, Generic)

instance FromJSON UserImg
instance ToJSON UserImg
