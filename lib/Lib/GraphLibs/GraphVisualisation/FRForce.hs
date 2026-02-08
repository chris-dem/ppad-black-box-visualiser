{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}

module Lib.GraphLibs.GraphVisualisation.FRForce where

import Lib.GraphLibs.GraphClass
import Lib.GraphLibs.GraphVisualisation.GVisClass

import Control.Monad.Loops (whileM_)
import Control.Monad.Random
import Control.Monad.State
import Data.Complex (magnitude)
import Data.Map qualified as M
import GHC.Generics
import Linear.V2 qualified as L
import Optics
import System.Random

data FRForce = FRForce
    { distConstraint :: Float
    , forceLimit :: Float
    , itLimit :: Int
    }
    deriving (Generic, Show, Eq)

instance GraphVisualisation FRForce where
    graphVisU :: (UGraphClass g, RandomGen gen) => FRForce -> gen -> g -> M.Map (VertexType g) (L.V2 Float)
    graphVisU fr gen graph = flip evalRand gen $ flip execStateT (0, M.empty) $ do
        forM_ (vertexList graph) $ \x -> do
            a <- lift $ getRandomR (-1.0, 1.0)
            b <- lift $ getRandomR (-1.0, 1.0)
            modify (M.insert x (L.V2 a b))
        whileM_ cond body
      where
        cond = do
            mag <- gets (max . map magnitude . snd)
            it <- gets (fst)
            it < fr ^. itLimit && mag fr ^. #forceLimit

        body = undefined

    graphVisD :: (DGraphClass g, RandomGen gen) => FRForce -> gen -> g -> M.Map (VertexType g) (L.V2 Float)
    graphVisD = undefined
