{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

-- Book: Thinking with Types

module Lib.HList where

import Data.List (intercalate)
import Data.Proxy
import Data.String
import GHC.TypeNats
import Unsafe.Coerce (unsafeCoerce)

data VSNat (n :: Nat) where
    SZ :: VSNat 0
    SS :: VSNat n -> VSNat (n + 1)

toVSInt :: VSNat n -> Int
toVSInt SZ = 0
toVSInt (SS x) = 1 + toVSInt x

data Vec (n :: Nat) t where
    FNil :: Vec 0 t
    (:#) :: t -> Vec n t -> Vec (n + 1) t

toList :: Vec a t -> [t]
toList FNil = []
toList (t :# ts) = t : toList ts

indexVS :: (i + 1 <= n) => VSNat i -> Vec n x -> x
indexVS SZ (h :# _) = h
indexVS (SS n) (_ :# h) = indexVS n h

vconc :: Vec n a -> Vec m a -> Vec (n + m) a
FNil `vconc` a = a
(x :# xs) `vconc` ys = x :# (xs `vconc` ys)

instance (Show t, KnownNat n) => Show (Vec n t) where
    show FNil = "[]"
    show xs = "Vec(" ++ show len ++ "): [" ++ intercalate "," shwXs ++ "]"
      where
        shwXs = map show $ toList xs
        len = fromIntegral $ natVal (Proxy @n)

instance Functor (Vec n) where
    fmap _ FNil = FNil
    fmap f (x :# xs) = f x :# fmap f xs

instance Foldable (Vec n) where
    foldMap f FNil = mempty
    foldMap f (x :# xs) = mappend left right
      where
        left = f x
        right = foldMap f xs

vrepeat' :: VSNat n -> a -> Vec n a
vrepeat' SZ x = FNil
vrepeat' (SS a) x = x :# vrepeat' a x

vrepeat :: forall n a. (KnownNat n) => a -> Vec n a
vrepeat = vrepeat' len
  where
    go :: Natural -> VSNat n
    go 0 = unsafeCoerce SZ
    go k = unsafeCoerce $ SS $ go (k - 1)
    len = go (natVal (Proxy @n))

instance (KnownNat n) => Applicative (Vec n) where
    pure = vrepeat
    (<*>) = vecAp

vecAp :: Vec n (a -> b) -> Vec n a -> Vec n b
vecAp FNil FNil = FNil
vecAp (f :# fs) (x :# xs) = f x :# vecAp fs xs

instance Traversable (Vec n) where
    traverse _ FNil = pure FNil
    traverse f (x :# xs) = (:#) <$> p <*> lp
      where
        p = f x
        lp = traverse f xs
