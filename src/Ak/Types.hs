module Ak.Types
    ( Task(priority, description)
    , task
    )
where

data Task =
    Task { priority :: Int
         , description :: String
         }
    deriving (Show)

task :: Int -> String -> Task
task = Task