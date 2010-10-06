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
import System.Directory
    ( getCurrentDirectory
    , canonicalizePath
    , doesFileExist
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
import Ak.Common
    ( taskFilename
    )
import qualified Ak.Commands as Commands

findTaskFile :: IO (Maybe FilePath)
findTaskFile = do
  let findTaskFile' dir =
          if dir == "/" then return Nothing else
              do
                let full = dir </> taskFilename
                e <- doesFileExist full
                if e then
                    return $ Just full else
                    (canonicalizePath (dir </> "..")) >>= findTaskFile'

  cur <- getCurrentDirectory
  findTaskFile' cur

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
  mPath <- findTaskFile

  let commands = Commands.allCommands
      abort = usage commands >> exitFailure
      onCommandError :: Command -> CommandError -> IO ()
      onCommandError cmd (CommandError msg) = do
         putStrLn $ "Error running command '" ++ cmdName cmd ++ "': " ++ msg

  when (null args) abort

  let (commandName:commandArgs) = args

  case lookupCommand commandName commands of
    Nothing -> abort
    Just cmd -> cmdHandler cmd mPath commandArgs
                `catch` (onCommandError cmd)
