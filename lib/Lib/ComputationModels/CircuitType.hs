{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Lib.CircuitType where

import Data.Foldable1 qualified as S
import Data.Kind (Type)
import Data.List (intercalate)
import Data.Proxy
import Data.Sequence qualified as S
import GHC.TypeNats
import Lib.HList
import Text.Printf (printf)

data CircuitFunc (k :: Nat) (n :: Nat) where
    EmptyCircuit :: CircuitFunc 0 n
    Input :: (i + 1 <= n) => VSNat i -> CircuitFunc (k - 1) n -> CircuitFunc k n
    AndStatement :: ((1 <= i), (1 <= j), (i <= k - 1), (j <= k - 1)) => VSNat i -> VSNat j -> CircuitFunc (k - 1) n -> CircuitFunc k n
    OrStatement :: ((1 <= i), (1 <= j), (i <= k - 1), (j <= k - 1)) => VSNat i -> VSNat j -> CircuitFunc (k - 1) n -> CircuitFunc k n
    NotStatement :: (i <= k - 1, 1 <= i) => VSNat i -> CircuitFunc (k - 1) n -> CircuitFunc k n

instance (KnownNat k, KnownNat n) => Show (CircuitFunc k n) where
    show x = (printf "[CircuitFunc %d %d: " kVal nVal) ++ "(" ++ intercalate ", " ((reverse . show') x) ++ ")" ++ "]"
      where
        kVal = natVal (Proxy @k)
        nVal = natVal (Proxy @n)

show' :: CircuitFunc k n -> [String]
show' EmptyCircuit = ["0"]
show' (Input x y) = printf "Input %d" ind : show' y
  where
    ind = evalVS x
show' (AndStatement x y r) = printf "%d * %d" indL indR : show' r
  where
    indL = evalVS x
    indR = evalVS y
show' (OrStatement x y r) = printf "%d + %d" indL indR : show' r
  where
    indL = evalVS x
    indR = evalVS y
show' (NotStatement x r) = printf "! %d" indL : show' r
  where
    indL = evalVS x

newtype BoolCircuit (n :: Nat) = BoolCircuit (CircuitFunc (n + 1) n)

instance BooleanFunction (CircuitFunc k) where
    apply s b = final
      where
        (a S.:|> final) = evalCirc s b S.empty

evalCirc :: CircuitFunc k n -> Vec n Bool -> S.Seq Bool -> S.Seq Bool
evalCirc EmptyCircuit _ v = v
evalCirc (Input s r) b v = evalCirc r b v S.:|> indexVS s b
evalCirc (AndStatement firstInd secInd rest) b v = m S.:|> result
  where
    m = evalCirc rest b v
    i1 = evalVS firstInd - 1
    i2 = evalVS secInd - 1
    result = m `S.index` i1 && m `S.index` i2
evalCirc (OrStatement firstInd secInd rest) b v = m S.:|> result
  where
    m = evalCirc rest b v
    i1 = evalVS firstInd - 1
    i2 = evalVS secInd - 1
    result = m `S.index` i1 || m `S.index` i2
evalCirc (NotStatement firstInd rest) b v = m S.:|> result
  where
    m = evalCirc rest b v
    i1 = evalVS firstInd - 1
    result = not $ m `S.index` i1
