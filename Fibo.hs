module Fibo where

-- Because DP is incomplete without an example of Fibonacci

fib n = fibs !! n
  where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
