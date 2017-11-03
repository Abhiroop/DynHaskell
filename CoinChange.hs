{-# LANGUAGE DeriveFunctor #-}
module CoinChange where

import Control.Arrow ((>>>))

newtype Term f = In {out :: f (Term f)}

data Attr f a = Attr
              { attribute :: a
              , hole      :: f (Attr f a)}

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

change :: Cent -> Int
change = undefined
