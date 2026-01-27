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
import Data.Sequence qualified as S
import GHC.TypeNats
import Lib.HList

class BooleanFunction (f :: Nat -> Type) where
    apply :: f a -> Vec a Bool -> Bool

data FormulaFunc (n :: Nat) where
    FAnd :: FormulaFunc n -> FormulaFunc n -> FormulaFunc n
    FOr :: FormulaFunc n -> FormulaFunc n -> FormulaFunc n
    FNot :: FormulaFunc n -> FormulaFunc n
    FNode :: (i + 1 <= n) => VSNat i -> FormulaFunc n

instance BooleanFunction FormulaFunc where
    apply (FAnd l r) b = apply l b && apply r b
    apply (FOr l r) b = apply l b || apply r b
    apply (FNot l) b = not $ apply l b
    apply (FNode e) b = indexVS e b

data CircuitFunc (k :: Nat) (n :: Nat) where
    EmptyCircuit :: CircuitFunc 0 n
    Input :: (i + 1 <= n) => VSNat i -> CircuitFunc (k - 1) n -> CircuitFunc k n
    AndStatement :: ((1 <= i), (1 <= j), (i <= k - 1), (j <= k - 1)) => VSNat i -> VSNat j -> CircuitFunc (k - 1) n -> CircuitFunc k n
    OrStatement :: ((1 <= i), (1 <= j), (i <= k - 1), (j <= k - 1)) => VSNat i -> VSNat j -> CircuitFunc (k - 1) n -> CircuitFunc k n
    NotStatement :: (i <= k - 1, 1 <= i) => VSNat i -> CircuitFunc (k - 1) n -> CircuitFunc k n

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
    i1 = toVSInt firstInd
    i2 = toVSInt secInd
    result = m `S.index` i1 && m `S.index` i2
evalCirc (OrStatement firstInd secInd rest) b v = m S.:|> result
  where
    m = evalCirc rest b v
    i1 = toVSInt firstInd
    i2 = toVSInt secInd
    result = m `S.index` i1 || m `S.index` i2
evalCirc (NotStatement firstInd rest) b v = m S.:|> result
  where
    m = evalCirc rest b v
    i1 = toVSInt firstInd
    result = not $ m `S.index` i1
