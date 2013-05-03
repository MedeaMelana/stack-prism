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
aap = Iso f g
  where
    f t = Just (Aap :- t)
    g (Aap :- t) = Just t

aap' :: forall t. Iso t (Aap :- t)
aap' = Iso f g
  where
    f :: forall a x t. (Generic a, MkIso (Rep a)) => IsoLhs (Rep a) x t -> Maybe (a :- t)
    --f :: forall x t'. IsoLhs (Rep Aap) x t' -> Maybe (Aap :- t')
    f t = Just (mapHead to (mkR t))

    g :: forall x. (Aap :- t) -> Maybe (IsoLhs (Rep Aap) x t)
    --g :: forall a x. Generic a => (a :- t)
    g (x :- t) = Just (mkL (from x :- t))


-- f: pattern functor
-- p: datatype parameter
-- t: type of the Iso stack tail


class MkIso (f :: * -> *) where
  type IsoLhs (f :: * -> *) (p :: *) (t :: *) :: *
  mkR :: IsoLhs f p t -> (f p :- t)
  mkL :: (f p :- t) -> IsoLhs f p t

instance MkIso U1 where
  type IsoLhs U1 p t = t
  mkR t         = U1 :- t
  mkL (U1 :- t) = t

instance MkIso f => MkIso (M1 i c f) where
  type IsoLhs (M1 i c f) p t = IsoLhs f p t
  mkR = mapHead M1 . mkR
  mkL = mkL . mapHead unM1

mapHead :: (a -> b) -> (a :- t) -> (b :- t)
mapHead f (h :- t) = f h :- t
