{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module ImageAPI where

import Servant.API
import DataTypes
import Codec.Picture.Types
import Servant.JuicyPixels
import qualified Data.Text as T

type UserId = Integer
type ImageId = T.Text


type ImageAPI =
       "user" :> "postImg"
              :> Capture "userId" UserId
              :> ReqBody '[JPEG 100] DynamicImage
              :> Post '[JSON] ImageId
  :<|> "image" :> Capture "imgId" ImageId
               :> QueryParam "size" T.Text
               :> Get '[JPEG 100] DynamicImage
  
