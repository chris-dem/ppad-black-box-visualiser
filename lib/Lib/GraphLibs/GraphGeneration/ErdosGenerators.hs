{-# LANGUAGE ScopedTypeVariables #-}

module Lib.GraphLibs.GraphGeneration.ErdosGenerators where

import Lib.GraphLibs.GraphClass
import Lib.GraphLibs.GraphGeneration.GraphGeneratorClass

import Control.Monad
import Control.Monad.Random.Lazy
import Control.Monad.State.Lazy
import System.Random

newtype ErdosRenyi = ErdosRenyi (Int, Float)
    deriving (Show, Eq)

mkErdosRenyi :: Int -> Float -> Maybe ErdosRenyi
mkErdosRenyi n p
    | n <= 0 || p <= 0 || p > 1 = Nothing
    | otherwise = Just $ ErdosRenyi (n, p)

type MyMonad b g = StateT b (Rand g)

instance GraphGenerator ErdosRenyi where
    genGraphU :: forall g m. (UGraphClass g, (FromInt (VertexType g)), RandomGen m) => ErdosRenyi -> m -> g
    genGraphU (ErdosRenyi (n, p)) gen = flip evalRand gen $ flip execStateT mempty $ do
        forM_ [0 .. n - 1] $ \v -> modify (`addNode` fromInt v)
        forM_ [(i, j) | i <- [0 .. n - 1], j <- [i + 1 .. n - 1]] $ \(i, j) -> do
            shouldAdd <- lift $ (< p) <$> getRandom
            when shouldAdd $ modify $ edge (fromInt i) (fromInt j)
      where
        edge b c a = addEdge a b c

    genGraphD :: (DGraphClass b, (RandomGen m)) => a -> m -> b
    genGraphD = undefined
