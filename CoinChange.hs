{-# LANGUAGE DeriveFunctor #-}
module CoinChange where

import Control.Arrow ((>>>))
import Prelude hiding (lookup)

newtype Term f = In {out :: f (Term f)}

data Attr f a = Attr
              { attribute :: a
              , hole      :: f (Attr f a)}

-- Course of value algebra
type CVAlgebra f a = f (Attr f a) -> a

histo :: Functor f => CVAlgebra f a -> Term f -> a
histo h = out >>> fmap worker >>> h where
  worker t = Attr (histo h t) (fmap worker (out t))

type Cent = Int

coins :: [Cent]
coins = [50, 25, 10, 5, 1]

data Nat a
    = Zero
    | Next a
      deriving Functor

-- integer to foldable
expand :: Int -> Term Nat
expand 0 = In Zero
expand n = In (Next (expand (n - 1)))

-- foldable to integer
compress :: Nat (Attr Nat a) -> Int
compress Zero              = 0
compress (Next (Attr _ x)) = 1 + compress x

change :: Cent -> Int
change amt = histo go (expand amt) where
  go :: CVAlgebra Nat Int -- <=> Nat (Attr Nat Int) -> Int
  go Zero = 1
  go curr@(Next attr) = let
    given      = compress curr
    validCoins = filter (<= given) coins
    remaining = map (given -) validCoins
    zeroCount = length (filter (== 0) remaining)
    others = sum (map (lookup attr) remaining)
    in zeroCount + others

lookup :: Attr Nat a -> Int -> a
lookup cache 0 = attribute cache
lookup cache n = lookup inner (n - 1)
  where (Next inner) = hole cache
