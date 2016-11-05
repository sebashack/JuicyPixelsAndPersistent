{-# LANGUAGE TypeOperators #-}

module Main where

import UserAPI
import ImageAPI
import UserInterpreter
import ImageInterpreter
import Configs
import Servant
import Servant.Server
import Network.Wai
import Network.Wai.Handler.Warp


type CombinedAPI = UserAPI :<|> ImageAPI


combinedServer config = userServer config :<|> imageServer config

serviceAPI :: Proxy CombinedAPI
serviceAPI = Proxy


main :: IO () 
main = do
  env <- getAppEnv
  port <- getAppPort
  let config = getConfig env
      server = combinedServer config
      logger = setApiLogger env
      app = serve serviceAPI server
  putStrLn $ "API running on port " ++ (show port)    
  run port $ logger app
