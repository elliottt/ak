module Ak.Commands
    ( allCommands
    , initialize
    , addTask
    , showTasks
    )
where

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
          case args of
            [p, s] -> appendTask path $ task (read p) s
            _ -> throwCommandError addTask "Expected 2 args"

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