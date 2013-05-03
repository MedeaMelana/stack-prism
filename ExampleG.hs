{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}

module ExampleG where

import Data.Iso
import GHC.Generics


data Aap = Aap deriving Generic

aap :: Iso t (Aap :- t)
aap = giso

giso :: forall a t. (Generic a, MkIso (Rep a)) => Iso (IsoLhs (Rep a) t) (a :- t)
giso = Iso f g
  where
    f :: forall a t'. (Generic a, MkIso (Rep a)) =>
          IsoLhs (Rep a) t' -> Maybe (a :- t')
    f t = Just (mapHead to (mkR t))

    g :: forall a t. (Generic a, MkIso (Rep a)) =>
          (a :- t) -> Maybe (IsoLhs (Rep a) t)
    g (x :- t) = Just (mkL (from x :- t))


-- f: pattern functor
-- p: datatype parameter
-- t: type of the Iso stack tail


class MkIso (f :: * -> *) where
  type IsoLhs (f :: * -> *) (t :: *) :: *
  mkR :: forall p t. IsoLhs f t -> (f p :- t)
  mkL :: forall p t. (f p :- t) -> IsoLhs f t

instance MkIso U1 where
  type IsoLhs U1 t = t
  mkR t         = U1 :- t
  mkL (U1 :- t) = t

instance MkIso f => MkIso (M1 i c f) where
  type IsoLhs (M1 i c f) t = IsoLhs f t
  mkR = mapHead M1 . mkR
  mkL = mkL . mapHead unM1

mapHead :: (a -> b) -> (a :- t) -> (b :- t)
mapHead f (h :- t) = f h :- t
