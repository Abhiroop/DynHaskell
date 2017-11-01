module Fibo where

-- Because DP is incomplete without an example of Fibonacci
import Data.Array

fib n = fibs !! n
  where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fib' max = go max
  where go 0 = 0
        go 1 = 1
        go n = fibs ! (n - 1) + fibs ! (n - 2)
        fibs = listArray (0, max) [go x | x <- [0..max]]

