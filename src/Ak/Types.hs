module Ak.Types
    ( Priority
    , Descr
    , Task(priority, description)
    , task
    )
where

type Priority = Int
type Descr    = String

data Task =
    Task { priority    :: Priority
         , description :: Descr
         }
    deriving (Show)

task :: Priority -> Descr -> Task
task = Task
