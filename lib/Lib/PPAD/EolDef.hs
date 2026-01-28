{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Lib.PPAD.EolDef where

import Data.Foldable1 qualified as S
import Data.Kind (Type)
import Data.List (intercalate)
import Data.Proxy
import Data.Sequence qualified as S
import GHC.TypeNats
import Lib.ComputationModels.ClassDefs
import Lib.HList
import Text.Printf (printf)

data EOLInstance (n :: Nat) where
    MkEOL :: ((MultiBooleanFunction f), (MultiBooleanFunction g)) => f n n -> g n n -> EOLInstance n

-- mkValidEOL :: ((BooleanFunction f), (BooleanFunction g)) => f n -> g n -> EOLInstance n
-- mkValidEOL f g = _

getSucc :: EOLInstance n -> Vec n Bool -> Maybe (Vec n Bool)
getSucc (MkEOL f g) b = if g `mapply` sB == b then Just sB else Nothing
  where
    sB = f `mapply` b

getPred :: EOLInstance n -> Vec n Bool -> Maybe (Vec n Bool)
getPred (MkEOL f g) = getSucc (MkEOL g f)
