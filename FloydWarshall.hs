module FloydWarshall where

import Data.IntMap.Lazy
import Data.Array
import Prelude hiding (lookup)
import Data.Maybe
-- data IntMap Array
-- naiveFW n = d n n n
--   where d i j 0 =

inf = maxBound :: Int

{-
n1 [(n1,0) (n2,8) (n3,2) (n4,inf)]
n2 [(n2,8) (n2,0) (n3,inf) (n4,1)]
n3 [(n1,2) (n2,inf)(n3,0) (n4,3)]
n4 [(n1,inf) (n2,1) (n3,3) (n4,0)]

d i j = min ((d i j k) , (d i k+1 k) + (d k+1 j k))
-}

type X = Int
type Y = Int

type Graph = IntMap (Array Y Int)

graph :: Graph
graph = let
  c1 = listArray (1,4) [0,8,2,inf]
  c2 = listArray (1,4) [8,0,inf,1]
  c3 = listArray (1,4) [2,inf,0,3]
  c4 = listArray (1,4) [inf,1,3,0]
  in fromList [(1,c1),(2,c2),(3,c3),(4,c4)]

(!>) :: Graph -> X -> Y -> Int
(!>) g x y = ((g Data.IntMap.Lazy.! x) Data.Array.! y)

shortestPaths :: Graph -> Graph
shortestPaths g = g--d i j k
  where (i,j,k) = (size g, size g, size g)
        d i j 0 = graph !> i j
        d i j k = undefined
