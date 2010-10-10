{-# LANGUAGE DeriveDataTypeable #-}

module Ak.Commands
    ( Command(cmdName, cmdUsage, cmdHandler, cmdDescription)
    , CommandHandler
    , CommandError(CommandError)
    , throwCommandError
    , command
    , allCommands
    , initializeCmd
    , addTaskCmd
    , showTasksCmd
    )
where

import Control.Exception
    ( Exception
    , throw
    )
import Control.Monad
    ( when
    , forM_
    )
import Data.Typeable
    ( Typeable
    )
import System.Directory
    ( getCurrentDirectory
    )
import Ak.Common
    ( taskFilename
    )
import Ak.Db
    ( DbSpec
    , loadDb
    , writeDb
    , Tasks(getTasks,addTask)
    , TaskStore()
    , makeRoot
    , emptyTaskStore
    , emptyDb
    , pushTaskStore
    , topTaskStore
    , taskStorePath
    )
import Ak.Tasks
    ( groupByPriority
    )
import Ak.Types
    ( Task(description)
    , task
    )

type CommandHandler = DbSpec -> [String] -> IO ()

data Command =
    Command { cmdName :: String
            , cmdUsage :: String
            , cmdDescription :: String
            , cmdHandler :: CommandHandler
            }

data CommandError =
    CommandError String deriving (Typeable, Show)

instance Exception CommandError

command :: String -> String -> String -> CommandHandler -> Command
command = Command

throwCommandError :: String -> IO a
throwCommandError msg = throw $ CommandError msg

allCommands :: [Command]
allCommands = [ initializeCmd
              , addTaskCmd
              , showTasksCmd
              ]

initializeArgs :: [String] -> TaskStore -> TaskStore
initializeArgs ["--root"] = makeRoot
initializeArgs _          = id

initializeCmd :: Command
initializeCmd =
  command "init"
          "init"
          "Initialize a new task file in the current directory"
           handler
  where
  handler spec args = do
    cwd <- getCurrentDirectory
    db  <- loadDb spec
    let isRoot = initializeArgs args
        newTs  = isRoot (emptyTaskStore cwd)
        write  = writeDb (pushTaskStore newTs db)
    case topTaskStore db of
      Nothing -> write
      Just tsTop
        | taskStorePath tsTop == cwd -> return ()
        | otherwise                  -> write

addTaskCmd :: Command
addTaskCmd =
    let handler spec args = do
          when (length args /= 2) (throwCommandError "Expected 2 args")
          db <- loadDb spec
          let [pStr, s] = args
          case reads pStr of
            ((p, _):_) -> writeDb (addTask (task p s) db)
            _          -> throwCommandError "Priority must be an integer"

    in command "add"
           "add <priority> <task string>"
           "Add a task with the given priority (integer)"
           handler

showTasksCmd :: Command
showTasksCmd =
    let handler spec _ = do
          db  <- loadDb spec
          mapM_ printPriorityGroup (groupByPriority (getTasks db))

        printPriorityGroup (p, ts) = do
          putStrLn $ "Priority: " ++ (show p)
          forM_ ts $ \t ->
              putStrLn $ concat [ "  "
                                , description t
                                ]

    in command "list"
           "list"
           "List all available tasks"
           handler
