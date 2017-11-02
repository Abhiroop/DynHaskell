module Nexus where

data Tree a = Leaf a | Node [Tree a]

-- Hylomorphism = Unfold then fold
{-
hylo :: Functor f =>
          (f b -> b)  Algebra. Catamorphisms work on them
       -> (a -> f a)  Co-algebra. Anamorphisms work on them
       ->  a          original structure
       ->  b          final value
-}
fold' :: (Either a [b] -> b) -> Tree a -> b
fold' f t = case t of
             Leaf x  -> f (Left x)
             Node ts -> f (Right (map (fold' f) ts))

unfold' :: (b -> Either a [b]) -> b -> Tree a
unfold' g x = case g x of
               Left y   -> Leaf y
               Right xs -> Node (map (unfold' g) xs)


-- hylo f g = fold f . unfold g
--          Algebra               CoAlgebra
hylo' :: (Either a [c] -> c) -> (b -> Either a [b]) -> b -> c
hylo' f g x = case g x of
               Left y   -> f (Left y)
               Right xs -> f (Right (map (hylo' f g) xs))

fold :: (a -> b) -> ([b] -> b) -> Tree a -> b
fold f g (Leaf x) = f x
fold f g (Node ts)= g (map (fold f g) ts)

unfold :: (b -> Bool) -> (b -> a) -> (b -> [b]) -> b -> Tree a
unfold p v h x = if p x then Leaf (v x) else
                   Node (map (unfold p v h) (h x))

-- hylo x = if p x then f x else g (map hylo (h x))

data LTree a = LLeaf a | LNode a [LTree a]

fill :: (a -> b) ->  ([b] -> b) -> Tree a -> LTree b
fill f g = fold (lleaf f) (lnode g)

lleaf :: (t -> a) -> t -> LTree a
lleaf f x = LLeaf (f x)

lnode :: ([a] -> a) -> [LTree a] -> LTree a
lnode g ts = LNode (g (map label ts)) ts

label (LLeaf x)    = x
label (LNode x ts) = x

hylo :: ([a] -> b) -> ([b] -> b) -> ([a] -> [[a]]) -> [a] -> b
hylo f g h = fold f g . mkTree h
  where mkTree h = unfold single id h
        single = undefined 
