module Ak.Tasks (
    groupByPriority
  ) where

import Ak.Types

import Data.Function
    ( on )
import Data.List
    ( groupBy )


groupByPriority :: [Task] -> [(Priority,[Task])]
groupByPriority  = map upd . groupBy ((==) `on` priority)
  where
  upd ts@(t:_) = (priority t, ts)
  upd _        = error "groupByPriority: impossible"
