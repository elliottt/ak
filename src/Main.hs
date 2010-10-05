module Main where

import Control.Applicative
    ( (<$>)
    )
import System.Environment
    ( getArgs
    )
import System.Directory
    ( getHomeDirectory
    )
import System.FilePath
    ( FilePath
    , (</>)
    )
import Ak.Types
    ( task
    )
import Ak.IO
    ( appendTask
    , readTasks
    )

taskFile :: IO FilePath
taskFile = (</> ".ak-tasks") <$> getHomeDirectory

showTasks :: FilePath -> IO ()
showTasks path = do
  print =<< readTasks path `catch` (const $ return [])

usage :: IO ()
usage = do
  putStrLn "Usage: ak [<priority> <task description>]"

main :: IO ()
main = do
  args <- getArgs
  path <- taskFile

  case args of
    [] -> showTasks path
    [p, s] -> appendTask path $ task (read p) s
    _ -> usage