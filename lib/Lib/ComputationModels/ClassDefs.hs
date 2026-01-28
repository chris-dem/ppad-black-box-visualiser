{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Lib.ComputationModels.ClassDefs where

import Data.Kind (Type)
import Data.List (intercalate)
import GHC.TypeNats
import Lib.HList


class BooleanFunction (f :: Nat -> Type) where
    apply :: f a -> Vec a Bool -> Bool

class MultiBooleanFunction (f :: Nat -> Nat -> Type) where
    mapply :: f i o -> Vec i Bool -> Vec o Bool


