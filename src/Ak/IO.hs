module Ak.IO
    ( appendTask
    , readTasks
    )
where

import Control.Applicative
    ( (<$>)
    )
import Control.Monad
    ( when
    )
import Data.List
    ( sortBy
    , groupBy
    )
import Data.Maybe
    ( catMaybes
    )
import Ak.Types
    ( Task(..)
    , TaskCollection
    , task
    )
import System.FilePath
    ( FilePath
    )
import System.Directory
    ( doesFileExist
    )

serialize :: Task -> String
serialize tsk = pStr ++ " " ++ dStr
    where
      pStr = show $ priority tsk
      dStr = description tsk

unserialize :: String -> Maybe Task
unserialize s = do
  case reads s of
    [] -> Nothing
    ((p, rest):_) -> Just $ task p $ tail rest

appendTask :: FilePath -> Task -> IO ()
appendTask path tsk = do
  e <- doesFileExist path
  when (not e) $ writeFile path ""
  appendFile path (serialize tsk ++ "\n")

taskPriority :: Task -> Task -> Ordering
taskPriority a b = (priority a) `compare` (priority b)

priorityGroup :: Task -> Task -> Bool
priorityGroup a b = priority a == priority b

readTasks :: FilePath -> IO TaskCollection
readTasks path = do
  taskLines <- lines <$> readFile path
  let ts = catMaybes $ map unserialize taskLines
      sorted = sortBy taskPriority ts
      groups = groupBy priorityGroup sorted
  return [ (priority $ head g, g) | g <- groups ]