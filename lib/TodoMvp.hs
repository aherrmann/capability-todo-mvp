{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module TodoMvp
  ( runTodoMvp
  ) where

import Capability.Reader
import Control.Monad.Reader (ReaderT (..))
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

class Monad m => TaskList m where
  getTasks :: m [Task]

newtype TaskListReader m (a :: *) =
  TaskListReader { runTaskListReader :: m a }
  deriving newtype (Functor, Applicative, Monad)
instance
  ( Monad m
  , HasReader TaskList [Task] m
  ) => TaskList (TaskListReader m)
  where
    getTasks = TaskListReader $! ask @TaskList

newtype TodoMvpM m a = TodoMvpM (ReaderT [Task] m a)
  deriving newtype (Functor, Applicative, Monad)
  deriving TaskList via TaskListReader (MonadReader (ReaderT [Task] m))

runTodoMvpM :: [Task] -> TodoMvpM m a -> m a
runTodoMvpM r (TodoMvpM m) = runReaderT m r

type TodoMvpApi = "tasks" :> Get '[JSON] [Task]

todoMvpServer :: TaskList m => ServerT TodoMvpApi m
todoMvpServer = pure exampleTasks

todoMvpApi :: Proxy TodoMvpApi
todoMvpApi = Proxy

hoistTodoMvp :: ServerT TodoMvpApi (TodoMvpM m) -> ServerT TodoMvpApi m
hoistTodoMvp = hoistServer todoMvpApi (runTodoMvpM exampleTasks)

todoMvpApp :: Application
todoMvpApp = serve todoMvpApi $! hoistTodoMvp todoMvpServer

runTodoMvp :: IO ()
runTodoMvp = run 8081 todoMvpApp
