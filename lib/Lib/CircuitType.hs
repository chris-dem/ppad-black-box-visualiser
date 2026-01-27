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

import Data.Kind (Type)
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
    Input :: (i + 1 <= n) => VSNat i -> CircuitFunc k n -> CircuitFunc (k + 1) n
    AndStatement :: ((1 <= i), (1 <= j), (i + 1 <= k), (j + 1 <= k)) => VSNat i -> VSNat j -> CircuitFunc k n -> CircuitFunc (k + 1) n
    OrStatement :: ((1 <= i), (1 <= j), (i + 1 <= k), (j + 1 <= k)) => VSNat i -> VSNat j -> CircuitFunc k n -> CircuitFunc (k + 1) n
    NotStatement :: (i + 1 <= k, 1 <= i) => VSNat i -> CircuitFunc k n -> CircuitFunc (k + 1) n

newtype BoolCircuit (n :: Nat) = BoolCircuit (CircuitFunc (n + 1) n)

instance BooleanFunction (CircuitFunc k) where
    apply s b = evalCirc s b ()


evalCirc = undefined
