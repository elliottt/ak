{-# LANGUAGE DeriveDataTypeable #-}
module Ak.Types
    ( Priority
    , Descr
    , Task(priority, description)
    , TaskCollection
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

type TaskCollection = [(Priority, [Task])]

task :: Priority -> Descr -> Task
task = Task

