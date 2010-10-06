module Ak.Commands
    ( allCommands
    , initialize
    , addTask
    , showTasks
    )
where

import Control.Monad
    ( when
    , mapM_
    , forM_
    )
import Data.List
    ( sortBy
    , groupBy
    )
import Data.Maybe
    ( isNothing
    )
import Ak.Common
    ( taskFilename
    )
import Ak.Types
    ( Command(..)
    , CommandHandler
    , Task(priority, description)
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

requiresTaskFile :: (FilePath -> [String] -> IO ()) -> CommandHandler
requiresTaskFile strictHandler mPath args = do
    when (isNothing mPath) $
         throwCommandError "No task file found; please run 'ak init'."
    let Just path = mPath
    strictHandler path args

initialize :: Command
initialize =
    let handler _ _ = writeFile taskFilename ""
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
            ((p, _):_) -> appendTask path $ task p s
            _ -> throwCommandError "Priority must be an integer"

    in command "add"
           "add <priority> <task string>"
           "Add a task with the given priority (integer)"
           (requiresTaskFile handler)

taskPriority :: Task -> Task -> Ordering
taskPriority a b = (priority a) `compare` (priority b)

priorityGroup :: Task -> Task -> Bool
priorityGroup a b = priority a == priority b

showTasks :: Command
showTasks =
    let handler path _ = do
          ts <- (readTasks path `catch` (const $ return []))
          let sorted = sortBy taskPriority ts
              groups = groupBy priorityGroup sorted
          mapM_ printPriorityGroup groups

        printPriorityGroup ts = do
          putStrLn $ "Priority: " ++ (show $ priority $ head ts)
          forM_ ts $ \t ->
              putStrLn $ concat [ "  "
                                , description t
                                ]

    in command "list"
           "list"
           "List all available tasks"
           (requiresTaskFile handler)