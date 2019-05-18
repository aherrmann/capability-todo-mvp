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
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad.IO.Class
import Control.Monad.Reader (ReaderT (..))
import Data.Aeson (ToJSON)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
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

newtype TaskListStm m (a :: *) =
  TaskListStm { runTaskListStm :: m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO)
instance
  ( MonadIO m
  , HasReader TaskList (TVar Int, TVar (IntMap Task)) m
  ) => TaskList (TaskListStm m)
  where
    getTasks = TaskListStm $! do
      (_, varMap) <- ask @TaskList
      fmap IntMap.elems $! liftIO $! readTVarIO varMap

newtype TodoMvpM m (a :: *) =
  TodoMvpM { runTodoMvpM :: (TVar Int, TVar (IntMap Task)) -> m a }
  deriving (Functor, Applicative, Monad, TaskList)
    via TaskListStm (MonadReader (ReaderT (TVar Int, TVar (IntMap Task)) m))

type TodoMvpApi = "tasks" :> Get '[JSON] [Task]

todoMvpServer :: TaskList m => ServerT TodoMvpApi m
todoMvpServer = pure exampleTasks

todoMvpApi :: Proxy TodoMvpApi
todoMvpApi = Proxy

hoistTodoMvp :: MonadIO m
  => ServerT TodoMvpApi (TodoMvpM m) -> ServerT TodoMvpApi m
hoistTodoMvp m = do
  varId <- liftIO $! newTVarIO 3
  varMap <- liftIO $! newTVarIO (IntMap.fromList $! zip [0..] exampleTasks)
  hoistServer todoMvpApi (flip runTodoMvpM (varId, varMap)) m

todoMvpApp :: Application
todoMvpApp = serve todoMvpApi $! hoistTodoMvp todoMvpServer

runTodoMvp :: IO ()
runTodoMvp = run 8081 todoMvpApp
