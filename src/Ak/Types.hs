{-# LANGUAGE DeriveDataTypeable #-}
module Ak.Types
    ( Priority
    , Descr
    , Task(priority, description)
    , TaskCollection
    , Command(cmdName, cmdUsage, cmdHandler, cmdDescription)
    , CommandHandler
    , CommandError(CommandError)
    , throwCommandError
    , command
    , task
    )
where

import Data.Typeable
    ( Typeable
    )
import Control.Exception
    ( Exception
    , throw
    )

type Priority = Int
type Descr    = String

data Task =
    Task { priority    :: Priority
         , description :: Descr
         }
    deriving (Show)

type TaskCollection = [(Priority, [Task])]

type CommandHandler = Maybe FilePath -> [String] -> IO ()

data Command =
    Command { cmdName :: String
            , cmdUsage :: String
            , cmdDescription :: String
            , cmdHandler :: CommandHandler
            }

data CommandError =
    CommandError String deriving (Typeable, Show)

instance Exception CommandError

throwCommandError :: String -> IO a
throwCommandError msg = throw $ CommandError msg

task :: Priority -> Descr -> Task
task = Task

command :: String -> String -> String -> CommandHandler -> Command
command = Command
