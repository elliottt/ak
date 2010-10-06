module Main where

import Prelude hiding (catch)
import Control.Exception (catch)

import Control.Applicative
    ( (<$>)
    )
import Control.Monad
    ( forM_
    , when
    )
import Data.Maybe
    ( listToMaybe
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
import System.Exit
    ( exitFailure
    )
import Ak.Types
    ( Command(..)
    , CommandError(..)
    )
import qualified Ak.Commands as Commands

taskFile :: IO FilePath
taskFile = (</> ".ak-tasks") <$> getHomeDirectory

usage :: [Command] -> IO ()
usage commands = do
  putStrLn "Usage: ak <command> [args]"
  forM_ commands $ \cmd ->
      putStrLn $ concat [ "  "
                        , cmdUsage cmd
                        , " - "
                        , cmdDescription cmd
                        ]

lookupCommand :: String -> [Command] -> Maybe Command
lookupCommand name commands =
    listToMaybe $ filter ((name ==) . cmdName) commands

main :: IO ()
main = do
  args <- getArgs
  path <- taskFile
  let commands = Commands.allCommands
      abort = usage commands >> exitFailure
      onCommandError :: Command -> CommandError -> IO ()
      onCommandError cmd (CommandError msg) = do
         putStrLn $ "Error running command '" ++ cmdName cmd ++ "': " ++ msg
         putStrLn $ "Usage: " ++ cmdUsage cmd

  when (null args) $ abort

  let (commandName:commandArgs) = args

  case lookupCommand commandName commands of
    Nothing -> abort
    Just cmd -> cmdHandler cmd path commandArgs
                `catch` (onCommandError cmd)
