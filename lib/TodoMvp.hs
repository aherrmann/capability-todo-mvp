{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module TodoMvp
  ( runTodoMvp
  ) where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp
import Servant

data Status =
    Open
  | Done
  deriving (Eq, Generic, Show)
  deriving anyclass ToJSON

data Task = Task
  { description :: Text
  , status :: Status
  }
  deriving (Eq, Generic, Show)
  deriving anyclass ToJSON

exampleTasks :: [Task]
exampleTasks =
  [ Task { description = "buy milk", status = Open }
  , Task { description = "buy cheese", status = Open }
  , Task { description = "buy stamps", status = Done }
  ]

type TodoMvpApi = "tasks" :> Get '[JSON] [Task]

todoMvpServer :: Server TodoMvpApi
todoMvpServer = pure exampleTasks

todoMvpApi :: Proxy TodoMvpApi
todoMvpApi = Proxy

todoMvpApp :: Application
todoMvpApp = serve todoMvpApi todoMvpServer

runTodoMvp :: IO ()
runTodoMvp = run 8081 todoMvpApp
