{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

-- Book: Thinking with Types

module Lib.HList where

import Data.List (intercalate)
import Data.Proxy
import Data.String
import Data.Type.Equality
import GHC.TypeNats
import Unsafe.Coerce (unsafeCoerce)

data VSNat (n :: Nat) where
    SZ :: VSNat 0
    S1 :: VSNat 1
    SS :: VSNat (Mod n 2) -> VSNat (Div n 2) -> VSNat n

mkSS ::
    forall n.
    (KnownNat n) =>
    VSNat (Mod n 2) ->
    VSNat (Div n 2) ->
    VSNat n

mkSS = unsafeCoerce (SS @n)

evalVS :: VSNat n -> Int
evalVS SZ = 0
evalVS S1 = 1
evalVS (SS m d) = evalVS m + 2 * evalVS d

vsFromType :: forall n. (KnownNat n) => VSNat n
vsFromType =
    case natVal (Proxy @n) of
        0 -> unsafeCoerce SZ
        1 -> unsafeCoerce S1
        k ->
            let modPart =
                    if even k
                        then unsafeCoerce SZ :: VSNat (Mod n 2)
                        else unsafeCoerce S1 :: VSNat (Mod n 2)
                divPart = unsafeCoerce (vsFromType @(Div n 2)) :: VSNat (Div n 2)
             in unsafeCoerce (mkSS @n modPart divPart)

instance Show (VSNat n) where
    show a = "[VSNat  " ++ reverse (showVS a) ++ "]"

showVS :: VSNat n -> String
showVS SZ = "0"
showVS S1 = "1"
showVS (SS a b) = showVS a ++ showVS b

data Vec (n :: Nat) t where
    FNil :: Vec 0 t
    (:#) :: t -> Vec n t -> Vec (n + 1) t
infixr 5 :#

instance Eq a => Eq (Vec n a) where
    FNil == FNil = True
    x :# xs == y :# ys = x == y && xs == ys

--
toList :: Vec a t -> [t]
toList FNil = []
toList (t :# ts) = t : toList ts

--
indexVS :: (i + 1 <= n) => VSNat i -> Vec n x -> x
indexVS SZ (h :# _) = h
indexVS S1 (_ :# m) = indexVS SZ m
indexVS a e = indexVS' (evalVS a) e

indexVS' :: Int -> Vec n x -> x
indexVS' 0 (a :# _) = a
indexVS' p (a :# as)
    | p < 0 = undefined
    | p > 0 = indexVS' (p - 1) (a :# as)

--
vconc :: Vec n a -> Vec m a -> Vec (n + m) a
FNil `vconc` a = a
(x :# xs) `vconc` ys = x :# el
  where
    el = xs `vconc` ys

instance (Show t, KnownNat n) => Show (Vec n t) where
    show FNil = "[]"
    show xs = "Vec(" ++ show len ++ "): [" ++ intercalate "," shwXs ++ "]"
      where
        shwXs = map show $ toList xs
        len = fromIntegral $ natVal (Proxy @n)

--
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
vrepeat' S1 x = x :# FNil
vrepeat' a x = vrepeatInt (evalVS a) x

vrepeatInt :: Int -> a -> Vec n a
vrepeatInt 0 a = unsafeCoerce FNil
vrepeatInt p a
    | p < 0 = undefined
    | p > 0 = unsafeCoerce $ a :# vrepeatInt (p - 1) a

vrepeat :: forall n a. (KnownNat n) => a -> Vec n a
vrepeat = vrepeat' len
  where
    len = vsFromType @n

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
