{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonoPatBinds #-}

module ExampleG where

import Data.Iso
import GHC.Generics


data Aap = Aap deriving (Generic, Show)

aap :: Iso t (Aap :- t)
aap = giso

data Noot = Noot Int deriving (Generic, Show)

noot :: Iso (Int :- t) (Noot :- t)
noot = giso

data Mies = Mies Int Char () deriving (Generic, Show)

--mies :: forall t. Iso (Int :- Char :- () :- t) (Mies :- t)
IsoList (Z mies) = wim (Mies 0 'c' ())

--true :: Iso t (Bool :- t)
--true = giso


wim :: a -> (Generic a, MkIsoList (Rep a)) => IsoList (Rep a) a
wim _ = mkIsoList to (Just . from)


class MkIsoList (f :: * -> *) where
  data IsoList (f :: * -> *) (a :: *)
  mkIsoList :: (f p -> a) -> (a -> Maybe (f q)) -> IsoList f a


instance MkIsoList f => MkIsoList (M1 D c f) where
  data IsoList (M1 D c f) a = IsoList (IsoList f a)
  mkIsoList f' g' = IsoList (mkIsoList (f' . M1) (fmap unM1 . g'))


instance MkIso f => MkIsoList (M1 C c f) where

  data IsoList (M1 C c f) a = Z (forall t. Iso (IsoLhs f t) (a :- t))

  mkIsoList f' g' = Z (Iso (f f') (g g'))
    where
      f :: forall a p t. (M1 C c f p -> a) -> IsoLhs f t -> Maybe (a :- t)
      f _f' lhs = Just (mapHead (_f' . M1) (mkR lhs))
      g :: forall a p t. (a -> Maybe (M1 C c f p)) -> (a :- t) -> Maybe (IsoLhs f t)
      g _g' (a :- t) = fmap (mkL . (:- t) . unM1) (_g' a)



-- Deriving types and conversions for single constructors

giso :: forall a t. (Generic a, MkIso (Rep a)) => Iso (IsoLhs (Rep a) t) (a :- t)
giso = Iso f g
  where
    f :: forall a t'. (Generic a, MkIso (Rep a)) =>
          IsoLhs (Rep a) t' -> Maybe (a :- t')
    f t = Just (mapHead to (mkR t))

    g :: forall a t. (Generic a, MkIso (Rep a)) =>
          (a :- t) -> Maybe (IsoLhs (Rep a) t)
    g (x :- t) = Just (mkL (from x :- t))

class MkIso (f :: * -> *) where
  type IsoLhs (f :: * -> *) (t :: *) :: *
  mkR :: forall p t. IsoLhs f t -> (f p :- t)
  mkL :: forall p t. (f p :- t) -> IsoLhs f t

instance MkIso U1 where
  type IsoLhs U1 t = t
  mkR t         = U1 :- t
  mkL (U1 :- t) = t

instance MkIso (K1 i a) where
  type IsoLhs (K1 i a) t = a :- t
  mkR (h :- t) = K1 h :- t
  mkL (K1 h :- t) = h :- t

instance MkIso f => MkIso (M1 i c f) where
  type IsoLhs (M1 i c f) t = IsoLhs f t
  mkR = mapHead M1 . mkR
  mkL = mkL . mapHead unM1

instance (MkIso f, MkIso g) => MkIso (f :*: g) where
  type IsoLhs (f :*: g) t = IsoLhs f (IsoLhs g t)
  mkR t = (hf :*: hg) :- tg
    where
      hf :- tf = mkR t
      hg :- tg = mkR tf
  mkL ((hf :*: hg) :- t) = mkL (hf :- mkL (hg :- t))



mapHead :: (a -> b) -> (a :- t) -> (b :- t)
mapHead f (h :- t) = f h :- t
