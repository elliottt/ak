module Ak.Commands
    ( allCommands
    , initialize
    , addTask
    , showTasks
    )
where

import Control.Monad
    ( when
    )
import Ak.Types
    ( Command(..)
    , throwCommandError
    , command
    , task
    )
import Ak.IO
    ( readTasks
    , appendTask
    )

allCommands :: [Command]
allCommands = [ initialize
              , addTask
              , showTasks
              ]

initialize :: Command
initialize =
    let handler _ args = do
          print args
    in command "init"
           "init"
           "Initialize a new task file in the current directory"
           handler

addTask :: Command
addTask =
    let handler path args = do
          when (length args /= 2) $
               throwCommandError "Expected 2 args"

          let [pStr, s] = args
          case reads pStr of
            ((p, _):_) -> appendTask path $ task (read p) s
            _ -> throwCommandError "Priority must be an integer"

    in command "add"
           "add <priority> <task string>"
           "Add a task with the given priority (integer)"
           handler

showTasks :: Command
showTasks =
    let handler path _ =
            (print =<< readTasks path `catch` (const $ return []))
    in command "list"
           "list"
           "List all available tasks"
           handler