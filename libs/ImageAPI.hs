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
  :<|> "user" :> "imgIds"
             :> Capture "userId" UserId
             :> Get '[JSON] [UserImg]
  :<|> "image" :> "image"
               :> Capture "imgId" ImageId
               :> Get '[JPEG 100] DynamicImage
  
