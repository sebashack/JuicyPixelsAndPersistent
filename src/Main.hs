{-# LANGUAGE TypeOperators #-}

module Main where

import UserAPI
import UserInterpreter
import Configs
import Servant
import Servant.Server
import Network.Wai
import Network.Wai.Handler.Warp


type CombinedAPI = UserAPI


combinedServer config = userServer config

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
