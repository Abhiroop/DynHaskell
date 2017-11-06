module FloydWarshall where

import Data.IntMap.Lazy as Map hiding (foldl')
import Data.List

type Vertex = Int
type Weight = Int

type Graph = IntMap (IntMap Weight)

weight :: Graph -> Vertex -> Vertex -> Maybe Weight
weight g i j = do
  jmap <- Map.lookup i g
  Map.lookup j jmap

shortestPaths :: [Vertex] -> Graph -> Graph
shortestPaths vs g = foldl' update g vs
  where
    update :: Graph -> Vertex -> Graph
    update = undefined
