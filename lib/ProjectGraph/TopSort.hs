module ProjectGraph.TopSort
    ( Dependency(..)
    , dependencyGraph
    , flipMap
    , leafIndices
    , topSort
    , unsafeLookUp
    )
    where

import           Data.Graph
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set

data Dependency k = Dependency k [k]

unsafeLookUp :: Ord k => k -> Map k a -> a
unsafeLookUp k m = let Just a = Map.lookup k m in a

flipMap :: (Ord k, Ord a) => Map k a -> Map a k
flipMap = Map.fromList . map (\(k, a) -> (a, k)) . Map.toList

leafIndices :: Ord k => [Dependency k] -> Map k Int
leafIndices = foldr (\(Dependency item prereqs) ->  (flip (foldr insertWithNewID)) (item : prereqs)) Map.empty
    where insertWithNewID k m = if k `Map.member` m then m else Map.insert k (Map.size m) m

dependencyGraph :: Ord k => Map k Int -> [Dependency k] -> Graph
dependencyGraph m deps =
    let edges = mconcat $ map
                            (\(Dependency item prereqs) ->
                                let itemIdx = unsafeLookUp item m
                                in map (flip (,) itemIdx) (helper m prereqs))
                            deps
    in buildG (0, Map.size m - 1) edges
    where helper m = map ((flip unsafeLookUp) m)
