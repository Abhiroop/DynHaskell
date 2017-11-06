This post is a literate Haskell file. To follow along, cut and paste the entire text of this post into a file called StarSemiring.lhs and load it up in GHCi.

> module StarSemiring where
I would like to introduce you to a very general algorithm that I like to call the Gauss-Jordan-Floyd-Warshall-McNaughton-Yamada algorithm. With this simple algorithm (an algorithm whose implementation is not very much longer than its name) you can solve almost half of the problems you might encounter in computer science. For instance, this algorithm will let you:

compute transitive closures
compute shortest paths
compute largest capacity paths
compute most reliable paths
compute the regular expression for a finite automaton
solve linear equations
This is all accomplished by working over an abstract mathematical structure called a *-semiring (read as "star-semiring"). We will see many examples of a *-semirings in this post implemented in literate Haskell.

> import Data.Array
> import Data.Maybe
> import Data.List
> import Control.Applicative
> import Control.Monad
>
> infixl 6 <+>
> infixl 7 <.>
First we define a semiring as a commutative monoid and a monoid satisfying the usual distributive and absorption laws.

> {- Laws:
>    a <+> b = b <+> a
>    (a <+> b) <+> c = a <+> (b <+> c)
>    a <+> zero  = zero <+> a  = a
>    (a <.> b) <.> c = a <.> (b <.> c)
>    a <.> one  = one <.> a  = a
>    a <.> zero = zero <.> a = zero
>    a <.> (b <+> c) = a <.> b <+> a <.> c
>    (a <+> b) <.> c = a <.> c <+> b <.> c
> -}
> class Semiring a where
>   zero  :: a
>   (<+>) :: a -> a -> a
>   one   :: a
>   (<.>) :: a -> a -> a
>   srsum :: [a] -> a
>   srsum = foldr (<+>) zero
>   srprod :: [a] -> a
>   srprod = foldr (<.>) one
Next we add one more method star (read as "asteration") to create a *-semiring.

> {- Laws:
>    star a = one <+> a <.> star a = one <+> star a <.> a
> -}
> class Semiring a => StarSemiring a where
>   star :: a -> a
A derived operation called "plus" can be defined in terms of star by x+ := xx∗. Actually the two operations are interdefinable because x∗ = 1 + x+.

>   star a = one <+> plus a
>   plus :: a -> a
>   plus a = a <.> star a
Most of our examples of a *-semiring will be instances of a more specialized structure called a Kleene algebra.

> {- Laws:
>    a <+> a = a
>    a <.> x <+> x = x  ==>  star a <.> x <+> x = x
>    x <.> a <+> x = x  ==>  x <.> star a <+> x = x
> -}
> class StarSemiring a => KleeneAlgebra a where
The most important examples of a *-semirings for us will be matrix *-semirings. Let us make a quick type for square matrices indexed by i

> data Edge i = i :-> i deriving (Eq, Ord, Bounded, Ix)
>
> newtype Matrix i e = Matrix (Array (Edge i) e)
>
> matrix :: (Ix i, Bounded i) => (Edge i -> e) -> Matrix i e
> matrix f = Matrix . listArray (minBound, maxBound) . map f $ entireRange
Our smart constructor matrix builds square matrix indexed over all elements of i from a given function f.

All of our matrices for a given index type are all the same size and thus Matrix i is an applicative functor. We postpone the obvious Applicative instances to the end as well as some pretty printing code.

All matrices over a *-semirings are themselves *-semirings. The additive operation for matrices simply lifts the additive operation from the underlying *-semiring.

> instance (Ix i, Bounded i, Semiring a) =>
>          Semiring (Matrix i a) where
>   zero = pure zero
>   (<+>) = liftA2 (<+>)
The multiplication operation for a *-semiring is the usual multiplication operation for matrices.

>   one = matrix (\(i :-> j) -> if i == j then one else zero)
>   Matrix x <.> Matrix y = matrix build
>    where
>     build (i :-> j) = srsum [x!(i :-> k) <.> y!(k :-> j) | k <- entireRange]
The final operation, star, is the heart of this post. The asteration operation solves the equation: x∗ = 1 + xx∗. Expending this recurrence equation out we see that asteration is a solution to the infinite series: x∗ = 1 + x + x2 + ….

Unfortunately we cannot just sum an infinite series, so instead we implement plus using the famous Gauss-Jordan-Floyd-Warshall-McNaughton-Yamada algorithm.

> instance (Ix i, Bounded i, StarSemiring a) =>
>          StarSemiring (Matrix i a) where
>   plus x = foldr f x entireRange
>    where
>     f k (Matrix m) = matrix build
>      where
>       build (i :-> j) = m!(i :-> j) <+>
>                         m!(i :-> k) <.> star (m!(k :-> k)) <.> m!(k :-> j)
And we are done. Well almost. We will also note that matricies over a Kleene algebra is itself a Kleene algebra.

> instance (Ix i, Bounded i, KleeneAlgebra a) =>
>          KleeneAlgebra (Matrix i a) where
Now, let us look at some applications of this algorithm. Our first example will be computing the reflexive-transitive closure of a directed graph. We will represent directed graph by its adjacency matrix. We build our own version of the Booleans called Connection

> data Connection = Connected | Unconnected deriving Eq
>
> type Graph i = Matrix i Connection
>
> graph :: (Ix i, Bounded i) => [Edge i] -> Graph i
> graph edgeList = matrix build
>  where
>   build i | i `elem` edgeList = Connected
>           | otherwise         = Unconnected
We will give a small example of a directed graph over five nodes.



> data Node = A | B | C | D | E deriving (Eq, Ord, Bounded, Ix, Show)
>
> exampleGraph :: Graph Node
> exampleGraph = graph [(A :-> B), (B :-> C), (C :-> D), (C :-> E), (D :-> B), (E :-> D)]
We can print the adjacency matrix of this example directed graph.

*StarSemiring> printMatrix exampleGraph
0 * 0 0 0
0 0 * 0 0
0 0 0 * *
0 * 0 0 0
0 0 0 * 0
To compute the reflexive-transitive closure of this directed graph we will turn Connection into our first example of a *-semiring. The additive operation is parallel composition of connections and the multiplicative operation is the sequential composition of connections.

> instance Semiring Connection where
>   zero = Unconnected
>   Connected   <+> _ = Connected
>   Unconnected <+> x = x
>   one = Connected
>   Unconnected <.> _ = Unconnected
>   Connected   <.> x = x
Asteration is the solution to x∗ = 1 + xx∗, but, for Connections, 1 plus anything is 1. Therefore the asteration of anything is one.

> instance StarSemiring Connection where
>   star _ = one
We note that this Connection *-semiring is also a Kleene algebra.

> instance KleeneAlgebra Connection where
Now that Connections are a *-semiring, that means matrices over Connections is a *-semiring. In particular, Graph Node is also a *-semiring. We can now compute the reflexive-transitive closure of our example directed graph using the asteration operation.

*StarSemiring> printMatrix . star $ exampleGraph
* * * * *
0 * * * *
0 * * * *
0 * * * *
0 * * * *
If you want just the transitive closure we simply compute x+.

*StarSemiring> printMatrix . plus $ exampleGraph
0 * * * *
0 * * * *
0 * * * *
0 * * * *
0 * * * *
This is great for deciding if two nodes are connected or not; however it does not tell us how to get from one node to another. To denote paths through a directed graph we will need to label the edges. For directed graphs with at most one edge from a source node to a destination node, we can simply label the edge by the two endpoints. For directed multigraphs we would need to assign names some other way, but this simple labeling method will do for our example.

> type LabeledGraph i = Matrix i (Maybe (Edge i))
>
> labelGraph :: (Ix i, Bounded i) => Graph i -> LabeledGraph i
> labelGraph m = f <$> m <*> matrix id
>  where
>   f Connected   l = Just l
>   f Unconnected _ = Nothing
>
> connect :: (Ix i) => Matrix i (Maybe a) -> Graph i
> connect = fmap (maybe Unconnected (const Connected))
Let us look at our labeled adjacency matrix.

*StarSemiring> printMatrix . labelGraph $ exampleGraph
Nothing Just (AB) Nothing   Nothing   Nothing
Nothing Nothing   Just (BC) Nothing   Nothing
Nothing Nothing   Nothing   Just (CD) Just (CE)
Nothing Just (DB) Nothing   Nothing   Nothing
Nothing Nothing   Nothing   Just (ED) Nothing
We see that each edge in the graph is label with the pair of its source node and its target node. When there is no edge between a pair of nodes, the adjacency matrix is marked with Nothing.

There could be an infinite number of paths from a given starting node to a target node. However, we can represent this infinite number of paths in a finite way by using regular expressions. Regular expressions will be our third example of a *-semiring (the second example was matrices of Connections). First we make a type for expressions of *-semirings over a given set of variable names.

> data StarSemiringExpression a
>   = Var a
>   | Or (StarSemiringExpression a) (StarSemiringExpression a)
>   | Seq (StarSemiringExpression a) (StarSemiringExpression a)
>   | Star (StarSemiringExpression a)
>   | None
>   | Empty
The type of regular expressions over a given set of variables will be a copy of this type of *-semiring expressions; however regular expressions will have its own instance of equality and other operations, so we give it a new type.

> newtype RE a = RE (StarSemiringExpression a)
>
> re :: a -> RE a
> re = RE . Var
>
> instance Eq a => Eq (RE a) where
>   (RE x) == (RE y) = (todo)
An algorithm for deciding if two regular expressions are equal is beyond the scope of this blog post. The problem is PSPACE complete, so it can be quite slow. We will not need to compare regular expressions in this post so we can skip this implementation for now and instead focus on the operations for the *-semiring.

While we could simply implement *-semiring operations directly as the constructors of StarSemiringExpression, instead we will take advantage of this opportunity to implement a few local simplifications: identities of ε and 0, absorption of 0, idempotency of asteration, and the following theorems of regular expressions:

ε + ε = ε
ε + x∗ = x∗
x∗ + ε = x∗
> instance Semiring (RE a) where
>   zero = RE None
>   RE None <+> y = y
>   x <+> RE None = x
>   RE Empty <+> RE Empty    = RE Empty
>   RE Empty <+> RE (Star y) = RE (Star y)
>   RE (Star x) <+> RE Empty = RE (Star x)
>   RE x <+> RE y            = RE (x `Or` y)
>   one = RE Empty
>   RE Empty <.> y = y
>   x <.> RE Empty = x
>   RE None <.> _  = RE None
>   _ <.> RE None  = RE None
>   RE x <.> RE y  = RE (x `Seq` y)
>
> instance StarSemiring (RE a) where
>   star (RE None)     = RE Empty
>   star (RE Empty)    = RE Empty
>   star (RE (Star x)) = star (RE x)
>   star (RE x)        = RE (Star x)
We note that regular expressions are Kleene algebras. In fact, they are the mother of all Kleene algebras, which we will make use of later.

> instance KleeneAlgebra (RE a) where
Now we can transform our labeled graph into a matrix of regular expressions.

> reGraph :: (Ix i) => Matrix i (Maybe a) -> Matrix i (RE a)
> reGraph = fmap (maybe zero re)
*StarSemiring> printMatrix . reGraph . labelGraph $ exampleGraph
0 (AB) 0    0    0
0 0    (BC) 0    0
0 0    0    (CD) (CE)
0 (DB) 0    0    0
0 0    0    (ED) 0
Then we can compute all the regular expressions for the paths through these edges starting from and ending with any given nodes. The regular expression we compute is not going to be minimal in general. In fact, the resulting matrix is a bit too big to fit in this blog so I will leave it for you to try to compute printMatrix . star . reGraph . labelGraph $ exampleGraph yourself.

If you look again at what we have done, you will see that we can view the graph as a finite automaton. What we have done is computed all the languages accepted by that finite automaton for every pair of possible initial and final states. Although we only had at most one edge between any pair of nodes in our example, if we instead create a directed multigraph and label it with tokens accepted by that edge, the asteration operation will still compute the regular expression of the language accepted for every pair of initial and final nodes. In fact, we can label edges with arbitrary regular expressions. In particular we can express ε-NFAs.

Next, let us look at the question from the title of this post: how to compute the shortest path between two nodes in a graph. We will take our example graph from the Wikipedia page for Dijkstra's algorithm. We label the edges with their weights.

> data Node2 = N1 | N2 | N3 | N4 | N5 | N6
>              deriving (Eq, Ord, Bounded, Ix, Show)
>
> exampleEdgeList2 :: (Edge Node2) -> Maybe Integer
> exampleEdgeList2 (i :-> j) =
>   (lookup (i :-> j) edges) `mplus` (lookup (j :-> i) edges)
>  where
>   edges = [(N1 :-> N2, 7), (N1 :-> N3, 9), (N1 :-> N6,14)
>           ,(N2 :-> N3,10), (N2 :-> N4,15)
>           ,(N3 :-> N4,11), (N3 :-> N6, 2)
>           ,(N4 :-> N5, 6)
>           ,(N5 :-> N6, 9)
>           ]
> exampleGraph2 :: Matrix Node2 (Maybe Integer)
> exampleGraph2 = matrix exampleEdgeList2
To compute the shortest path we will define a tropical *-semiring. A tropical semiring over non-negative numbers has minimum as the additive operation, and addition as the multiplicative operation. We add positive infinity as the identity element for minimum.

> data Tropical a = Tropical a -- only non-negative values allowed
>                 | Infinity deriving (Eq, Ord)
>
> instance (Ord a, Num a) => Semiring (Tropical a) where
>   zero = Infinity
>   Infinity     <+> y            = y
>   x            <+> Infinity     = x
>   (Tropical a) <+> (Tropical b) = Tropical (min a b)
>   one = Tropical 0
>   Infinity <.> _ = Infinity
>   _ <.> Infinity = Infinity
>   (Tropical x) <.> (Tropical y) = Tropical (x + y)
In a tropical semiring the multiplicative identity, one, is 0 which is the smallest element possible. This means the asteration operation is the constant one.

> instance (Ord a, Num a) => StarSemiring (Tropical a) where
>   star _ = one
We note that the tropical *-semiring is a Kleene algebra.

> instance (Ord a, Num a) => KleeneAlgebra (Tropical a) where
We can convert the labels in our example matrix to tropical values. The asteration of this tropical matrix will then tell us the minimum distance between any two nodes in the graph.

*StarSemiring> printMatrix . fmap (maybe zero Tropical) $ exampleGraph2
∞  7  9  ∞  ∞ 14
7  ∞  10 15 ∞ ∞
9  10 ∞  11 ∞ 2
∞  15 11 ∞  6 ∞
∞  ∞  ∞  6  ∞ 9
14 ∞  2  ∞  9 ∞

*StarSemiring> printMatrix . star . fmap (maybe zero Tropical) $ exampleGraph2
0  7  9  20 20 11
7  0  10 15 21 12
9  10 0  11 11 2
20 15 11 0  6  13
20 21 11 6  0  9
11 12 2  13 9  0
Again, this only tells us what the minimum distance between two nodes is. It does not tell us what a minimal path is. To find what the minimal paths are, we need to annotate our values with ancillary data. We create a new type called ShortestPath to contain this annotation.

> data ShortestPath a b = ShortestPath (Tropical a) b
>
> instance Functor (ShortestPath a) where
>   fmap f (ShortestPath a x) = ShortestPath a (f x)
>
> extract :: ShortestPath a b -> b
> extract (ShortestPath _ x) = x
When we compute the additive operation of the shortest path we will take ancillary data corresponding to the smaller tropical value. In case both tropical values are equal, then we take both pieces of ancillary data together by adding them. The multiplicative operation is simply lifted from the tropical operation and the multiplicative operation on the ancillary data.

> instance (Ord a, Num a, Semiring b) => Semiring (ShortestPath a b) where
>   zero = ShortestPath zero zero
>   ShortestPath a x <+> ShortestPath b y | c < b = ShortestPath a x
>                                         | c < a = ShortestPath b y
>                                         | otherwise = ShortestPath c (x <+> y)
>    where
>     c = a <+> b
>   one = ShortestPath one one
>   ShortestPath a x <.> ShortestPath b y = ShortestPath (a <.> b) (x <.> y)
The star operation simply returns one (which is the tropical value 0) in almost all cases. However, when the tropical value is already one (which is the tropical value 0), we can freely sequence this value as many times as we want. Therefore, in this case we return the asteration of the ancillary data.

> instance (Ord a, Num a, StarSemiring b) => StarSemiring (ShortestPath a b) where
>   star (ShortestPath x b) | x == one  = ShortestPath one (star b)
>                           | otherwise = ShortestPath one one
We note that the resulting structure is a Kleene algebra (I think) when the ancillary data is.

> instance (Ord a, Num a, KleeneAlgebra b) => KleeneAlgebra (ShortestPath a b) where
If we annotate our example graph with edge names for regular expressions we can compute the regular expression corresponding to all the shortest paths in a graph.

> annotate :: (Ix i, Bounded i, Ord a, Num a, Semiring b) =>
>             ((Edge i) -> b) -> Matrix i (Maybe a) -> Matrix i (ShortestPath a b)
> annotate f m = go <$> m <*> labelGraph (connect m)
>  where
>   go v e = ShortestPath (maybe zero Tropical v) (maybe zero f e)
*StarSemiring> printMatrix . star . annotate re $ exampleGraph2
ε[0]                   (N1N2)[7]                           (N1N3)[9]        (N1N3)(N3N4)[20] (N1N3)(N3N6)(N6N5)[20]              (N1N3)(N3N6)[11]
(N2N1)[7]              ε[0]                                (N2N3)[10]       (N2N4)[15]       (N2N4)(N4N5)|(N2N3)(N3N6)(N6N5)[21] (N2N3)(N3N6)[12]
(N3N1)[9]              (N3N2)[10]                          ε[0]             (N3N4)[11]       (N3N6)(N6N5)[11]                    (N3N6)[2]
(N4N3)(N3N1)[20]       (N4N2)[15]                          (N4N3)[11]       ε[0]             (N4N5)[6]                           (N4N3)(N3N6)[13]
(N5N6)(N6N3)(N3N1)[20] (N5N4)(N4N2)|(N5N6)(N6N3)(N3N2)[21] (N5N6)(N6N3)[11] (N5N4)[6]        ε[0]                                (N5N6)[9]
(N6N3)(N3N1)[11]       (N6N3)(N3N2)[12]                    (N6N3)[2]        (N6N3)(N3N4)[13] (N6N5)[9]                           ε[0]
Having a regular expression of paths is nice, but what if we want to just find one shortest path. What we can do is compute a lazy list of all shortest paths and take the first one. We call a lazy list of lists of labels a Language and it is our next example of a *-semiring.

> newtype Language a = Language [[a]] deriving Show
>
> letter x = Language [[x]]
The *-semiring operations on a language are ones for regular languages.

> instance Semiring (Language a) where
>   zero = Language []
>   (Language x) <+> (Language y) = Language (x `interleave` y)
>    where
>     []     `interleave` ys = ys
>     (x:xs) `interleave` ys = x:(ys `interleave` xs)
>   one = Language (pure [])
>   (Language x) <.> (Language y) = Language (dovetail (++) x y)
>    where
>     dovetail f l1 l2 = concat $ go l1 (scanl (flip (:)) [] l2)
>      where
>       go [] _           = []
>       go l1 l2@(x:y:ys) = (zipWith f l1 x):(go l1 (y:ys))
>       go l1@(a:as) [x]  = (zipWith f l1 x):(go as [x])
>
> instance StarSemiring (Language a) where
>   star (Language l) = one <+> plusList (filter (not . null) l)
>    where
>     plusList [] = zero
>     plusList l  = star (Language l) <.> (Language l)
Here we allow the representation of languages to contain repeated strings. With some care I think should be possible to eliminate repeated strings and perhaps even make the stream sorted.

Again, we note that Languages are a Kleene algebra.

> instance KleeneAlgebra (Language a) where
Now we can annotate our ShortestPath with a language instead of a regular expression and extract the first shortest path, if it exists.

> someWord :: Language a -> Maybe [a]
> someWord (Language l) = listToMaybe l
*StarSemiring> printMatrix . fmap (someWord . extract) . star . annotate letter $ exampleGraph2
Just []                     Just [(N1N2)]        Just [(N1N3)]        Just [(N1N3),(N3N4)] Just [(N1N3),(N3N6),(N6N5)] Just [(N1N3),(N3N6)]
Just [(N2N1)]               Just []              Just [(N2N3)]        Just [(N2N4)]        Just [(N2N4),(N4N5)]        Just [(N2N3),(N3N6)]
Just [(N3N1)]               Just [(N3N2)]        Just []              Just [(N3N4)]        Just [(N3N6),(N6N5)]        Just [(N3N6)]
Just [(N4N3),(N3N1)]        Just [(N4N2)]        Just [(N4N3)]        Just []              Just [(N4N5)]               Just [(N4N3),(N3N6)]
Just [(N5N6),(N6N3),(N3N1)] Just [(N5N4),(N4N2)] Just [(N5N6),(N6N3)] Just [(N5N4)]        Just []                     Just [(N5N6)]
Just [(N6N3),(N3N1)]        Just [(N6N3),(N3N2)] Just [(N6N3)]        Just [(N6N3),(N3N4)] Just [(N6N5)]               Just []
This is one very general way of computing shortest paths. By varying the *-semiring, you can compute largest capacity paths, and most reliable paths in the same way.

Recall that I said that regular expressions were the mother of all Kleene algebras. What I mean by this is that regular expressions can interpret any other Kleene algebra if we have an interpretation for the variables.

> evalRE :: (KleeneAlgebra a) => (l -> a) -> RE l -> a
> evalRE f (RE None)        = zero
> evalRE f (RE Empty)       = one
> evalRE f (RE (Var a))     = f a
> evalRE f (RE (Star x))    = star (evalRE f (RE x))
> evalRE f (RE (x `Or` y))  = (evalRE f (RE x)) <+> (evalRE f (RE y))
> evalRE f (RE (x `Seq` y)) = (evalRE f (RE x)) <.> (evalRE f (RE y))
This means that instead of computing the asteration of a matrices of different values that we are interested in, we can compute one matrix of regular expressions and then interpret that matrix in any Kleene algebra to get the same result. This is quite useful if we want to look at many different Kleene algebra interpretations of a single graph because we only have to compute the asteration of a matrix once. However, the resulting regular expressions are often large, so if you are interested in only one Kleene algebra interpretation, it can be faster to compute it directly.

*StarSemiring> printMatrix . fmap (evalRE Tropical) . star . reGraph $ exampleGraph2
0  7  9  20 20 11
7  0  10 15 21 12
9  10 0  11 11 2
20 15 11 0  6  13
20 21 11 6  0  9
11 12 2  13 9  0
The last example I want to show is how to solve linear equations. This will also be our first example of a *-semiring that is not a Kleene algebra. If we take the one point compactification of the real line, we can turn it into a *-semiring. The additive and multiplicative operations are the usual addition and multiplication operations. However we need to take care that 0 absorbs all elements under multiplication, including ∞.

> data Compact a = Real a
>                | Inf
>
> instance (Eq a, Num a) => Semiring (Compact a) where
>   zero = Real 0
>   Inf    <+> _      = Inf
>   _      <+> Inf    = Inf
>   Real x <+> Real y = Real (x + y)
>   one = Real 1
>   Real 0 <.> _      = Real 0
>   _      <.> Real 0 = Real 0
>   Inf    <.> _      = Inf
>   _      <.> Inf    = Inf
>   Real x <.> Real y = Real (x * y)
The asteration of a number is the solution to x∗ = 1 + xx∗, which by simple algebra can be x∗ := (1 - x)-1. We define 1∗ := ∞, and ∞∗ := ∞ to satisfy the recursion equation.

> instance (Eq a, Fractional a) => StarSemiring (Compact a) where
>   star (Real 1) = Inf
>   star (Real x) = Real (recip (1 - x))
>   star Inf      = Inf
Matrix asteration solves fixpoints of affine equations. Notice that A∗B = (AA∗ + 1)B, and hence A∗B = AA∗B + B. Therefore A∗B is a solution to the equation X = AX+ B, which is a very common problem in linear algebra.

Lets compute the asteration of an 2×2 example matrix.

> exampleMatrix :: Num a => Matrix Bool a
> exampleMatrix = matrix value
>  where
>   value (False :-> False) = 2
>   value (False :-> True ) = 1
>   value (True  :-> False) = 0
>   value (True  :-> True ) = 2
*StarSemiring> printMatrix . fmap Real $ exampleMatrix
2 1
0 2

*StarSemiring> printMatrix . star . fmap Real $ exampleMatrix
-1.0 1.0
0.0  -1.0
Asteration can be used to solve traditional problems in linear algebra too. Asteration can be used to invert a matrix A because (1 - A)∗ = A-1.

> inverse :: (Eq a, Ix i, Bounded i, Fractional a) =>
>            Matrix i a -> Matrix i (Compact a)
> inverse m = star (one <+> fmap (Real . negate) m)
*StarSemiring> printMatrix . inverse $ exampleMatrix
0.5 -0.25
0.0 0.5
This *-semiring is not a Kleene algebra, so we cannot deduce this asteration from a matrix of regular expressions. However we can deduce it from a matrix of *-semiring expressions. *-semiring expressions have the same syntax as regular expressions; however it has fewer laws. This means we can implement *-semiring expressions as a *-semiring in a similar manner to our implementation of regular expression. All we have to do is remove some of the simplifications that we had implemented for regular expressions.

> instance Semiring (StarSemiringExpression a) where
>   zero = None
>   None <+> y = y
>   x <+> None = x
>   x <+> y    = x `Or` y
>   one = Empty
>   Empty <.> y     = y
>   x     <.> Empty = x
>   None  <.> _     = None
>   _     <.> None  = None
>   x     <.> y     = x `Seq` y
>
> instance StarSemiring (StarSemiringExpression a) where
>   star None     = Empty
>   star x        = Star x
Similar to regular expressions, we can interpret any *-semiring expression in any *-semiring.

> evalSSE :: (StarSemiring a) => (l -> a) -> StarSemiringExpression l -> a
> evalSSE f None        = zero
> evalSSE f Empty       = one
> evalSSE f (Var a)     = f a
> evalSSE f (Star x)    = star (evalSSE f x)
> evalSSE f (x `Or` y)  = (evalSSE f x) <+> (evalSSE f y)
> evalSSE f (x `Seq` y) = (evalSSE f x) <.> (evalSSE f y)
Now, if we want to, we can save an intermediate matrix of *-semiring expressions and interpret it using real numbers, or any other *-semiring (including any Kleene algebra) later.

*StarSemiring> printMatrix . fmap (evalSSE Real) . star . fmap Var $ exampleMatrix
-1.0 1.0
0.0  -1.0
That it for this introduction to *-semirings and their applications. In the next installment we will see a faster method to compute the shortest path/regular expression for just a particular pair of source and target nodes by using eliminants.

References:

Transitive closure and related semiring properties via eliminants by S. Kamal Abdali and B. David Saunders
Algebraic Structures for Transitive Closure by Rafael Penaloza
Below you will find an appendix of functions that completes this module.
> entireRange :: (Ix i, Bounded i) => [i]
> entireRange = range (minBound, maxBound)
>
> instance (Ix i) => Functor (Matrix i) where
>   fmap f (Matrix m) = Matrix (fmap f m)
>
> instance (Ix i, Bounded i) => Applicative (Matrix i) where
>   pure x = matrix (const x)
>   Matrix f <*> Matrix x = matrix (\(i :-> j) -> (f!(i :-> j)) (x!(i :-> j)))
>
> transpose :: (Ix i, Bounded i) => Matrix i a -> Matrix i a
> transpose (Matrix m) = matrix (\(i :-> j) -> m!(j :-> i))
>
> showMatrix :: (Ix i, Bounded i, Show a) => Matrix i a -> String
> showMatrix (Matrix m) =
>   unlines [concat [pad (m'!(i :-> j)) j | j <- entireRange]
>           | i <- entireRange]
>  where
>   m' = fmap show m
>   lenm = fmap length m'
>   len j = maximum [lenm!(i :-> j) | i <- entireRange]
>   pad s j = s ++ replicate ((len j) - (length s) +1) ' '
>
> printMatrix :: (Ix i, Bounded i, Show a) => Matrix i a -> IO ()
> printMatrix = putStrLn . showMatrix
>
> instance Show Connection  where
>   show Connected   = "*"
>   show Unconnected = "0"
>
> instance Show a => Show (Edge a) where
>   showsPrec _ (i :-> j) = showParen True (shows i . shows j)
>
> instance Show a => Show (StarSemiringExpression a) where
>   showsPrec d (Var a) = showParen (d > 10) (shows a)
>   showsPrec d Empty = showParen (d > 10) (showString "ε")
>   showsPrec d None = showParen (d > 10) (showString "0")
>   showsPrec d (Star x) = showParen (d > 9) (showsPrec 9 x . showString "*")
>   showsPrec d (x `Or` y) = showParen (d > 6) showStr
>    where
>     showStr = showsPrec 6 x . showString "|" . showsPrec 6 y
>   showsPrec d (x `Seq` y) = showParen (d > 7) showStr
>    where
>     showStr = showsPrec 7 x . showsPrec 7 y
>
> instance Show a => Show (RE a) where
>   showsPrec d (RE x) = showsPrec d x
>
> instance Show a => Show (Tropical a) where
>   show (Tropical a) = show a
>   show Infinity = "∞"
>
> instance (Show a, Show b) => Show (ShortestPath a b) where
>   show (ShortestPath a x) = show x ++ "[" ++ show a ++ "]"
>
> instance (Show a) => Show (Compact a) where
>   show (Real a) = show a
>   show Inf = "∞"
>
> todo = error "TODO"
