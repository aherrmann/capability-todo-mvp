{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module TodoMvp
  ( runTodoMvp
  ) where

import Data.Text (Text)
import Network.Wai.Handler.Warp
import Servant

type TodoMvpApi = "hello" :> Get '[JSON] Text

todoMvpServer :: Server TodoMvpApi
todoMvpServer = pure "Hello world!"

todoMvpApi :: Proxy TodoMvpApi
todoMvpApi = Proxy

todoMvpApp :: Application
todoMvpApp = serve todoMvpApi todoMvpServer

runTodoMvp :: IO ()
runTodoMvp = run 8081 todoMvpApp
