module EditDistance where

import Data.Array as Array
import Data.List(minimumBy)
import Data.Ord(comparing)

naive a b = d (length a) (length b)
  where d i 0 = i
        d 0 j = j
        d i j
          | a !! (i - 1) == b !! (j -1) = d (i - 1) (j - 1)
          | otherwise = minimum [ d (i - 1) j         + 1
                                , d i      (j -1)     + 1
                                , d (i -1) (j - 1)    + 1]
basic a b = d m n
  where (m, n) = (length a, length b)
        d i 0 = i
        d 0 j = j
        d i j
          | a !! (i - 1) ==  b !! (j - 1) = ds ! (i - 1, j - 1)
          | otherwise = minimum [ ds ! (i - 1, j)     + 1
                                , ds ! (i, j - 1)     + 1
                                , ds ! (i - 1, j - 1) + 1
                                ]

        ds = Array.listArray bounds
               [d i j | (i, j) <- Array.range bounds]

        bounds = ((0, 0), (m, n))

better a b = d m n
  where (m,n) = (length a, length b)
        a'    = Array.listArray (1,m) a
        b'    = Array.listArray (1,n) b

        d i 0 = i
        d 0 j = j
        d i j
          | a' ! i == b' ! j = ds ! (i - 1, j - 1)
          | otherwise = minimum [ ds ! (i - 1, j)     + 1
                                , ds ! (i, j - 1)     + 1
                                , ds ! (i - 1, j - 1) + 1
                                ]

        ds = Array.listArray bounds
               [d i j | (i,j) <- Array.range bounds]

        bounds = ((0,0),(m,n))

data Action = None | Add | Remove | Modify
type Distance = Integer

script :: Eq a => (Action -> Distance) -> [a] -> [a] -> [Action]
script cost a b = reverse . snd $ d m n
  where (m, n) = (length a, length b)
        a'     = Array.listArray (1, m) a
        b'     = Array.listArray (1, n) b

        d 0 0 = (0, [])
        d i 0 = go (i - 1) 0 Remove
        d 0 j = go 0 (j - 1) Add
        d i j
          | a' ! i ==  b' ! j = go (i - 1) (j - 1) None
          | otherwise = minimum' [ go (i - 1) j       Remove
                                 , go i (j - 1)       Add
                                 , go (i - 1) (j - 1) Modify
                                 ]

        minimum' = minimumBy (comparing fst)
        go i j action = let (score, actions) = ds ! (i, j) in
          (score + cost action, action : actions)

        ds = Array.listArray bounds [d i j | (i, j) <- Array.range bounds]
        bounds = ((0, 0), (m, n))

cost :: Action -> Distance
cost None = 0
cost _    = 1
