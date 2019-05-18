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
import Data.Aeson (FromJSON, ToJSON)
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
  deriving anyclass (FromJSON, ToJSON)

data Task = Task
  { description :: Text
  , status :: Status
  }
  deriving (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

exampleTasks :: [Task]
exampleTasks =
  [ Task { description = "buy milk", status = Open }
  , Task { description = "buy cheese", status = Open }
  , Task { description = "buy stamps", status = Done }
  ]

class Monad m => TaskList m where
  getTasks :: m [(Int, Task)]
  createTask :: Task -> m Int

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
      fmap IntMap.toList $! liftIO $! readTVarIO varMap
    createTask newTask = TaskListStm $! do
      (varId, varMap) <- ask @TaskList
      liftIO $! atomically $! do
        newId <- stateTVar varId (\count -> (count, succ count))
        modifyTVar' varMap (IntMap.insert newId newTask)
        pure newId

newtype TodoMvpM m (a :: *) =
  TodoMvpM { runTodoMvpM :: (TVar Int, TVar (IntMap Task)) -> m a }
  deriving (Functor, Applicative, Monad, TaskList)
    via TaskListStm (MonadReader (ReaderT (TVar Int, TVar (IntMap Task)) m))

type TodoMvpApi =
  "tasks" :> Get '[JSON] [(Int, Task)]
  :<|> "tasks" :> ReqBody '[JSON] Task :> Post '[JSON] Int

todoMvpServer :: TaskList m => ServerT TodoMvpApi m
todoMvpServer = getTasks :<|> createTask

todoMvpApi :: Proxy TodoMvpApi
todoMvpApi = Proxy

hoistTodoMvp :: MonadIO m
  => (TVar Int, TVar (IntMap Task))
  -> ServerT TodoMvpApi (TodoMvpM m)
  -> ServerT TodoMvpApi m
hoistTodoMvp r = hoistServer todoMvpApi (flip runTodoMvpM r)

todoMvpApp :: (TVar Int, TVar (IntMap Task)) -> Application
todoMvpApp r = serve todoMvpApi $! (hoistTodoMvp r) todoMvpServer

runTodoMvp :: IO ()
runTodoMvp = do
  varId <- liftIO $! newTVarIO 3
  varMap <- liftIO $! newTVarIO (IntMap.fromList $! zip [0..] exampleTasks)
  run 8081 (todoMvpApp (varId, varMap))
