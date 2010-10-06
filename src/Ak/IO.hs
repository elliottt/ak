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
import Data.Maybe
    ( catMaybes
    )
import Ak.Types
    ( Task(..)
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

readTasks :: FilePath -> IO [Task]
readTasks path =
    catMaybes <$> map unserialize <$> lines <$> readFile path
