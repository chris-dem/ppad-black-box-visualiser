{-# LANGUAGE TypeFamilies #-}

module Lib.GraphLibs.GraphClass where

{-
 -  Consider CSR implementation for faster traversal
 -
 -  -}

import Data.Function ((&))
import Data.Kind (Type)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Set qualified as S

newtype USparseGraph v = USGraph (M.Map v (S.Set v))
    deriving (Eq, Show)

newtype UEdge v = UEdge (v, v) deriving (Show)
newtype DEdge v = DEdge (v, v) deriving (Ord, Show, Eq)

instance (Eq v) => Eq (UEdge v) where
    UEdge (a, b) == UEdge (c, d) = (a == c && b == d) || (b == c && a == d)

instance (Ord v) => Ord (UEdge v) where
    UEdge (a, b) <= UEdge (c, d) = x <= y
      where
        g :: (Ord a) => (a, a) -> (a, a)
        g (a, b) | a <= b = (a, b)
        g (a, b) = (b, a)

        x = g (a, b)
        y = g (c, d)

class (Monoid g) => GraphClass g where
    type VertexType g :: Type

    vSize :: g -> Int
    eSize :: g -> Int
    neigh :: g -> VertexType g -> Maybe (S.Set (VertexType g))
    vertexList :: g -> [VertexType g]
    addNode :: g -> VertexType g -> g
    addEdge :: g -> VertexType g -> VertexType g -> g

class (GraphClass g, Ord (VertexType g)) => UGraphClass g where
    edgeSetU :: g -> VertexType g -> S.Set (UEdge (VertexType g))

class (GraphClass g, Ord (VertexType g)) => DGraphClass g where
    inNeigh :: g -> VertexType g -> S.Set (VertexType g)
    outNeigh :: g -> VertexType g -> S.Set (VertexType g)
    edgeSetD :: g -> VertexType g -> S.Set (DEdge (VertexType g))

instance (Ord v) => Semigroup (USparseGraph v) where
    USGraph a <> USGraph b = USGraph $ a <> b

instance (Ord v) => Monoid (USparseGraph v) where
    mempty = USGraph M.empty

instance (Ord v) => GraphClass (USparseGraph v) where
    type VertexType (USparseGraph v) = v

    vSize (USGraph a) = M.size a
    eSize (USGraph a) = M.foldl (\acc x -> acc + S.size x) 0 a `div` 2
    neigh (USGraph a) v = a M.!? v

    vertexList (USGraph a) = M.keysSet a & S.toList
    addNode (USGraph a) v = USGraph $ M.insert v S.empty a
    addEdge (USGraph a) v1 v2 = M.adjust (S.insert v2) v1 a & M.adjust (S.insert v1) v2 & USGraph

instance (Ord v) => UGraphClass (USparseGraph v) where
    edgeSetU (USGraph l) v = (UEdge . (v,)) `S.map` fromMaybe S.empty (M.lookup v l)
