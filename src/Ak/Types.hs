{-# LANGUAGE DeriveDataTypeable #-}
module Ak.Types
    ( Priority
    , Descr
    , Task(priority, description)
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

type CommandHandler = FilePath -> [String] -> IO ()

data Command =
    Command { cmdName :: String
            , cmdUsage :: String
            , cmdDescription :: String
            , cmdHandler :: CommandHandler
            }

instance Show Command where
    show (Command name usage desc _) =
        concat [ name
               , "/"
               , usage
               , "/"
               , desc
               ]

data CommandError =
    CommandError Command String
                 deriving (Typeable, Show)

instance Exception CommandError

throwCommandError :: Command -> String -> IO a
throwCommandError cmd msg = throw $ CommandError cmd msg

task :: Priority -> Descr -> Task
task = Task

command :: String -> String -> String -> CommandHandler -> Command
command = Command
