{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}

module Data.Piso.Generic (mkPisoList, PisoList(..), PisoLhs) where

import Data.Piso
import GHC.Generics


-- | Derive a list of partial isomorphisms, one for each constructor in the 'Generic' datatype @a@. The list is wrapped in the unary constructor @PisoList@. Within that constructor, the isomorphisms are separated by the right-associative binary infix constructor @:&@. Finally, the individual isomorphisms are wrapped in the unary constructor @I@. These constructors are all exported by this module, but no documentation is generated for them by Hackage.
--
-- As an example, here is how to define the isomorphisms @nil@ and @cons@ for @[a]@, which is an instance of @Generic@:
--
-- > nil  :: Piso              t  ([a] :- t)
-- > cons :: Piso (a :- [a] :- t) ([a] :- t)
-- > (nil, cons) = (nil', cons')
-- >   where
-- >     PisoList (I nil' :& I cons') = mkPisoList
--
-- GHC 7.6.3 requires the extra indirection through @nil'@ and @cons'@, due to bug 7268 (<http://ghc.haskell.org/trac/ghc/ticket/7268>). When it is fixed, the example above can be written in a more direct way:
--
-- > nil  :: Piso              t  ([a] :- t)
-- > cons :: Piso (a :- [a] :- t) ([a] :- t)
-- > PisoList (I nil :& I cons) = mkPisoList
--
-- If you are familiar with the generic representations from @Data.Generic@, you might be interested in the exact types of the various constructors in which the isomorphisms are wrapped:
--
-- > I        :: (forall t. Piso (PisoLhs f t) (a :- t)) -> PisoList (M1 C c f) a
-- > (:&)     :: PisoList f a -> PisoList g a -> PisoList (f :+: g) a
-- > PisoList :: PisoList f a -> PisoList (M1 D c f) a
--
-- The type constructor @PisoLhs@ that appears in the type of @I@ is an internal type family that builds the proper heterogenous list of types (using ':-') based on the constructor's fields.
mkPisoList :: (Generic a, MkPisoList (Rep a)) => PisoList (Rep a) a
mkPisoList = mkPisoList' to (Just . from)


class MkPisoList (f :: * -> *) where
  data PisoList (f :: * -> *) (a :: *)
  mkPisoList' :: (f p -> a) -> (a -> Maybe (f q)) -> PisoList f a


instance MkPisoList f => MkPisoList (M1 D c f) where
  data PisoList (M1 D c f) a = PisoList (PisoList f a)
  mkPisoList' f' g' = PisoList (mkPisoList' (f' . M1) (fmap unM1 . g'))


infixr :&

instance (MkPisoList f, MkPisoList g) => MkPisoList (f :+: g) where
  data PisoList (f :+: g) a = PisoList f a :& PisoList g a
  mkPisoList' f' g' = f f' g' :& g f' g'
    where
      f :: forall a p q. ((f :+: g) p -> a) -> (a -> Maybe ((f :+: g) q)) -> PisoList f a
      f _f' _g' = mkPisoList' (\fp -> _f' (L1 fp)) (matchL _g')
      g :: forall a p q. ((f :+: g) p -> a) -> (a -> Maybe ((f :+: g) q)) -> PisoList g a
      g _f' _g' = mkPisoList' (\gp -> _f' (R1 gp)) (matchR _g')

      matchL :: (a -> Maybe ((f :+: g) q)) -> a -> Maybe (f q)
      matchL _g' a = case _g' a of
        Just (L1 f) -> Just f
        _ -> Nothing

      matchR :: (a -> Maybe ((f :+: g) q)) -> a -> Maybe (g q)
      matchR _g' a = case _g' a of
        Just (R1 g) -> Just g
        _ -> Nothing


instance MkPiso f => MkPisoList (M1 C c f) where

  data PisoList (M1 C c f) a = I (forall t cat. FromPiso cat => cat (PisoLhs f t) (a :- t))

  mkPisoList' f' g' = I (fromPiso (Piso (f f') (g g')))
    where
      f :: forall a p t. (M1 C c f p -> a) -> PisoLhs f t -> a :- t
      f _f' lhs = mapHead (_f' . M1) (mkR lhs)
      g :: forall a p t. (a -> Maybe (M1 C c f p)) -> (a :- t) -> Maybe (PisoLhs f t)
      g _g' (a :- t) = fmap (mkL . (:- t) . unM1) (_g' a)


-- Deriving types and conversions for single constructors

class MkPiso (f :: * -> *) where
  type PisoLhs (f :: * -> *) (t :: *) :: *
  mkR :: forall p t. PisoLhs f t -> (f p :- t)
  mkL :: forall p t. (f p :- t) -> PisoLhs f t

instance MkPiso U1 where
  type PisoLhs U1 t = t
  mkR t         = U1 :- t
  mkL (U1 :- t) = t

instance MkPiso (K1 i a) where
  type PisoLhs (K1 i a) t = a :- t
  mkR (h :- t) = K1 h :- t
  mkL (K1 h :- t) = h :- t

instance MkPiso f => MkPiso (M1 i c f) where
  type PisoLhs (M1 i c f) t = PisoLhs f t
  mkR = mapHead M1 . mkR
  mkL = mkL . mapHead unM1

instance (MkPiso f, MkPiso g) => MkPiso (f :*: g) where
  type PisoLhs (f :*: g) t = PisoLhs f (PisoLhs g t)
  mkR t = (hf :*: hg) :- tg
    where
      hf :- tf = mkR t
      hg :- tg = mkR tf
  mkL ((hf :*: hg) :- t) = mkL (hf :- mkL (hg :- t))



mapHead :: (a -> b) -> (a :- t) -> (b :- t)
mapHead f (h :- t) = f h :- t
