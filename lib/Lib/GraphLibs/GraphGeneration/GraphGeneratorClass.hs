module Lib.GraphLibs.GraphGeneration.GraphGeneratorClass where

import Lib.GraphLibs.GraphClass
import System.Random (RandomGen)

class FromInt a where
    fromInt :: Int -> a

newtype IdxInt = IdxInt Int
    deriving (Show, Eq, Ord)

instance FromInt IdxInt where
    fromInt = IdxInt

class GraphGenerator a where
    genGraphU :: (UGraphClass b, FromInt (VertexType b), (RandomGen m)) => a -> m -> b
    genGraphD :: (DGraphClass b, FromInt (VertexType b), (RandomGen m)) => a -> m -> b
