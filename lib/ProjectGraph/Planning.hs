module ProjectGraph.Planning
    ( Plan(..)
    , emptyPlan
    ) where

import qualified Data.Map.Strict as Map (empty)
import           ProjectGraph.Schema (Task, TaskMap)
import           ProjectGraph.TopSort (Dependency)

data Plan = Plan
    { messages :: [String]
    , taskMap :: TaskMap
    , deps :: [Dependency Task]
    }

emptyPlan :: Plan
emptyPlan = Plan [] Map.empty []
