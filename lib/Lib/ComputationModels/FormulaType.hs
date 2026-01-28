{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Lib.ComputationModels.FormulaType where

import Data.Foldable1 qualified as S
import Data.Kind (Type)
import Data.List (intercalate)
import Data.Proxy
import Data.Sequence qualified as S
import GHC.TypeNats
import Lib.ComputationModels.ClassDefs
import Lib.HList
import Text.Printf (printf)

data FormulaFunc (n :: Nat) where
    FAnd :: FormulaFunc n -> FormulaFunc n -> FormulaFunc n
    FOr :: FormulaFunc n -> FormulaFunc n -> FormulaFunc n
    FNot :: FormulaFunc n -> FormulaFunc n
    FNode :: (i + 1 <= n) => VSNat i -> FormulaFunc n

newtype MultiFormula (i :: Nat) (o :: Nat) = MF (Vec o (FormulaFunc i))

instance BooleanFunction FormulaFunc where
    apply (FAnd l r) b = apply l b && apply r b
    apply (FOr l r) b = apply l b || apply r b
    apply (FNot l) b = not $ apply l b
    apply (FNode e) b = indexVS e b

instance MultiBooleanFunction MultiFormula where
    mapply (MF FNil) b = FNil
    mapply (MF (f :# fs)) b = f `apply` b :# mapply (MF fs) b
