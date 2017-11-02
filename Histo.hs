module Histo where

import Control.Arrow

newtype Term f = In {out :: f (Term f)}

type Algebra f a = f a -> a

cata :: (Functor f) => Algebra f a -> Term f -> a
cata f = out >>> fmap (cata f) >>> f


type RAlgebra f a = f (Term f, a) -> a

para :: Functor f => RAlgebra f a -> Term f -> a
para f = out >>> fmap (id &&& para f) >>> f

data Attr f a = Attr
              { attribute :: a
              , hole      :: f (Attr f a)}

-- This is gold: http://blog.sumtypeofway.com/recursion-schemes-part-iv-time-is-of-the-essence/

-- Course of value algebra
type CVAlgebra f a = f (Attr f a) -> a


histo :: Functor f => CVAlgebra f a -> Term f -> a
histo h = out >>> fmap worker >>> h where
  worker t = Attr (histo h t) (fmap worker (out t))

