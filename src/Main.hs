module Main where

import Prelude hiding (catch)
import Control.Exception (catch)

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
import System.Exit
    ( exitFailure
    )
import Ak.Commands
    ( Command(..)
    , CommandError(..)
    )
import Ak.Db
    ( discoverDbSpec )
import qualified Ak.Commands as Commands

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
  spec <- discoverDbSpec

  let commands = Commands.allCommands
      abort = usage commands >> exitFailure
      onCommandError :: Command -> CommandError -> IO ()
      onCommandError cmd (CommandError msg) = do
         putStrLn $ "Error running command '" ++ cmdName cmd ++ "': " ++ msg

  when (null args) abort

  let (commandName:commandArgs) = args

  case lookupCommand commandName commands of
    Nothing -> abort
    Just cmd -> cmdHandler cmd spec commandArgs
                `catch` (onCommandError cmd)
