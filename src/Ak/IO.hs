module Ak.IO
    ( appendTask
    , readTasks
    , writeTask
    , readTask
    , serialize
    , unserialize
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
import System.IO
    ( hGetLine, hPutStrLn, Handle )

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

writeTask :: Handle -> Task -> IO ()
writeTask h = hPutStrLn h . serialize

readTask :: Handle -> IO (Maybe Task)
readTask h = unserialize `fmap` hGetLine h
