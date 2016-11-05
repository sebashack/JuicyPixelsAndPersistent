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

module ImageInterpreter (imageServer) where

import Servant
import ImageAPI
import Configs
import ActionRunner
import Control.Monad.Except
import Control.Monad.IO.Class
import Codec.Picture
import Codec.Picture.Extra
import Codec.Picture.Types
import Servant.JuicyPixels
import qualified Data.ByteString as  SB (ByteString, length) 
import qualified Data.ByteString.Lazy as LB (toStrict, fromStrict)
import qualified DataTypes as DT
import qualified Schemas as US
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Database.Persist as Per
import qualified Database.Persist.MySQL as MySQL
import qualified Data.Time as TM



postImg :: ConfigMySQL
        -> Integer
        -> DynamicImage
        -> ExceptT ServantErr IO T.Text
postImg config userId (ImageYCbCr8 imgJPEG) = do
  postImg <- liftIO $ runMySQL config  $ do
    let encodedImg = LB.toStrict (encodeJpegAtQuality 100 imgJPEG)
        numBytes = SB.length encodedImg
    case checkImgBound numBytes of
      GT -> return $ Left "Image Greater Than 5MB"
      LT -> return $ Left "Image Less Than 150KB"
      EQ -> do
        imgCode <- liftIO genImgCode
        let userKey = US.UserKey $ MySQL.SqlBackendKey $ fromIntegral userId
            image = US.Image imgCode userKey encodedImg
        Per.insert image
        return $ Right $ TE.decodeUtf8 imgCode
  either (\error -> throwError err400 { errBody = error }) (\v -> return v) postImg      
  where
    checkImgBound bytes
      | bytes < 150000 = LT
      | bytes > 5000000 = GT
      | otherwise = EQ
    genImgCode :: IO SB.ByteString
    genImgCode = do
      (TM.UTCTime date time) <- TM.getCurrentTime
      let (year, monthDay) = splitAt 4 (show date)
          (month, day') = splitAt 2 (tail monthDay)
          day = tail day'
          hour = show $ round (time * 1000000)
      return $ (TE.encodeUtf8 . T.pack) (year ++ month ++ day ++ hour)


 

getImg :: ConfigMySQL
       -> T.Text
       -> Maybe T.Text
       -> ExceptT ServantErr IO DynamicImage
getImg config imgCode size = do
  image <- liftIO $ runMySQL config  $ do
    maybeRepoImg <- Per.getBy $ US.ImgCode (TE.encodeUtf8 imgCode)
    case maybeRepoImg of
      Nothing -> return $ Left "Image Not Found"
      Just (Per.Entity key repoImg) -> do
        let blob = US.imageImage repoImg
        return $ decodeImg blob size
  case image of
    Left "Image Not Found" -> throwError err400 { errBody = "Image Not Found" }
    Left "Cannot Decode Image" -> throwError err500 { errBody = "Cannot Decode Image" }
    Left _ -> throwError err500 { errBody = "Unknown Error" }
    Right v -> return v       
  where
    scaleImg maybeSize img = case maybeSize of
      Just "mini" -> scaleBilinear 100 100 img
      Just "avatar" -> scaleBilinear 200 200 img
      Just "bigAvatar" -> scaleBilinear 250 250 img
      Just "medium" -> scaleBilinear 530 250 img
      Just "standard"-> scaleBilinear 840 460 img
      Just "large" -> scaleBilinear 920 540 img
      _ -> img
    decodeImg :: SB.ByteString -> Maybe T.Text -> Either String DynamicImage
    decodeImg bs imgSize =
      case decodeJpeg bs of
        Left _ -> Left "Cannot Decode Image"
        Right img ->
          let imgRGB8 = convertRGB8 img
              scaledImg = scaleImg imgSize imgRGB8
          in Right $ ImageRGB8 scaledImg   
              
      
imageServer :: ConfigMySQL -> Server ImageAPI
imageServer config = postImg config
               :<|>  getImg config
