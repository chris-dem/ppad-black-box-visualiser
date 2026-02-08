module Lib.GraphLibs.GraphVisualisation.GVisClass where

import Lib.GraphLibs.GraphClass (DGraphClass, GraphClass (VertexType), UGraphClass)

import Data.Map qualified as M
import Linear.V2 (V2)
import System.Random (RandomGen)

class GraphVisualisation vis where
    graphVisU :: (UGraphClass g, RandomGen gen) => vis -> gen -> g -> M.Map (VertexType g) (V2 Float)
    graphVisD :: (DGraphClass g, RandomGen gen) => vis -> gen -> g -> M.Map (VertexType g) (V2 Float)
