{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}

module Data.Iso.Generic (mkIsoList, IsoList(..)) where

import Data.Iso
import GHC.Generics


mkIsoList :: (Generic a, MkIsoList (Rep a)) => IsoList (Rep a) a
mkIsoList = mkIsoList' to (Just . from)


class MkIsoList (f :: * -> *) where
  data IsoList (f :: * -> *) (a :: *)
  mkIsoList' :: (f p -> a) -> (a -> Maybe (f q)) -> IsoList f a


instance MkIsoList f => MkIsoList (M1 D c f) where
  data IsoList (M1 D c f) a = IsoList (IsoList f a)
  mkIsoList' f' g' = IsoList (mkIsoList' (f' . M1) (fmap unM1 . g'))


instance (MkIsoList f, MkIsoList g) => MkIsoList (f :+: g) where
  data IsoList (f :+: g) a = IsoList f a :& IsoList g a
  mkIsoList' f' g' = f f' g' :& g f' g'
    where
      f :: forall a p q. ((f :+: g) p -> a) -> (a -> Maybe ((f :+: g) q)) -> IsoList f a
      f _f' _g' = mkIsoList' (\fp -> _f' (L1 fp)) (matchL _g')
      g :: forall a p q. ((f :+: g) p -> a) -> (a -> Maybe ((f :+: g) q)) -> IsoList g a
      g _f' _g' = mkIsoList' (\gp -> _f' (R1 gp)) (matchR _g')

      matchL :: (a -> Maybe ((f :+: g) q)) -> a -> Maybe (f q)
      matchL _g' a = case _g' a of
        Just (L1 f) -> Just f
        _ -> Nothing

      matchR :: (a -> Maybe ((f :+: g) q)) -> a -> Maybe (g q)
      matchR _g' a = case _g' a of
        Just (R1 g) -> Just g
        _ -> Nothing


instance MkIso f => MkIsoList (M1 C c f) where

  data IsoList (M1 C c f) a = Z (forall t. Iso (IsoLhs f t) (a :- t))

  mkIsoList' f' g' = Z (Iso (f f') (g g'))
    where
      f :: forall a p t. (M1 C c f p -> a) -> IsoLhs f t -> Maybe (a :- t)
      f _f' lhs = Just (mapHead (_f' . M1) (mkR lhs))
      g :: forall a p t. (a -> Maybe (M1 C c f p)) -> (a :- t) -> Maybe (IsoLhs f t)
      g _g' (a :- t) = fmap (mkL . (:- t) . unM1) (_g' a)


-- Deriving types and conversions for single constructors

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
