{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}

module Data.StackPrism.Generic (mkStackPrismList, StackPrisms, StackPrismList(..), StackPrismLhs) where

import Data.StackPrism
import GHC.Generics


-- | Derive a list of partial isomorphisms, one for each constructor in the 'Generic' datatype @a@. The list is wrapped in the unary constructor @StackPrismList@. Within that constructor, the isomorphisms are separated by the right-associative binary infix constructor @:&@. Finally, the individual isomorphisms are wrapped in the unary constructor @I@. These constructors are all exported by this module, but no documentation is generated for them by Hackage.
--
-- As an example, here is how to define the isomorphisms @nil@ and @cons@ for @[a]@, which is an instance of @Generic@:
--
-- > nil  :: StackPrism              t  ([a] :- t)
-- > cons :: StackPrism (a :- [a] :- t) ([a] :- t)
-- > (nil, cons) = (nil', cons')
-- >   where
-- >     StackPrismList (I nil' :& I cons') = mkStackPrismList
--
-- GHC 7.6.3 requires the extra indirection through @nil'@ and @cons'@, due to bug 7268 (<http://ghc.haskell.org/trac/ghc/ticket/7268>). When it is fixed, the example above can be written in a more direct way:
--
-- > nil  :: StackPrism              t  ([a] :- t)
-- > cons :: StackPrism (a :- [a] :- t) ([a] :- t)
-- > StackPrismList (I nil :& I cons) = mkStackPrismList
--
-- If you are familiar with the generic representations from @Data.Generic@, you might be interested in the exact types of the various constructors in which the isomorphisms are wrapped:
--
-- > I        :: (forall t. StackPrism (StackPrismLhs f t) (a :- t)) -> StackPrismList (M1 C c f) a
-- > (:&)     :: StackPrismList f a -> StackPrismList g a -> StackPrismList (f :+: g) a
-- > StackPrismList :: StackPrismList f a -> StackPrismList (M1 D c f) a
--
-- The type constructor @StackPrismLhs@ that appears in the type of @I@ is an internal type family that builds the proper heterogenous list of types (using ':-') based on the constructor's fields.
mkStackPrismList :: (Generic a, MkStackPrismList (Rep a)) => StackPrisms a
mkStackPrismList = mkStackPrismList' to (Just . from)

type StackPrisms a = StackPrismList (Rep a) a

class MkStackPrismList (f :: * -> *) where
  data StackPrismList (f :: * -> *) (a :: *)
  mkStackPrismList' :: (f p -> a) -> (a -> Maybe (f q)) -> StackPrismList f a


instance MkStackPrismList f => MkStackPrismList (M1 D c f) where
  data StackPrismList (M1 D c f) a = StackPrismList (StackPrismList f a)
  mkStackPrismList' f' g' = StackPrismList (mkStackPrismList' (f' . M1) (fmap unM1 . g'))


infixr :&

instance (MkStackPrismList f, MkStackPrismList g) => MkStackPrismList (f :+: g) where
  data StackPrismList (f :+: g) a = StackPrismList f a :& StackPrismList g a
  mkStackPrismList' f' g' = f f' g' :& g f' g'
    where
      f :: forall a p q. ((f :+: g) p -> a) -> (a -> Maybe ((f :+: g) q)) -> StackPrismList f a
      f _f' _g' = mkStackPrismList' (\fp -> _f' (L1 fp)) (matchL _g')
      g :: forall a p q. ((f :+: g) p -> a) -> (a -> Maybe ((f :+: g) q)) -> StackPrismList g a
      g _f' _g' = mkStackPrismList' (\gp -> _f' (R1 gp)) (matchR _g')

      matchL :: (a -> Maybe ((f :+: g) q)) -> a -> Maybe (f q)
      matchL _g' a = case _g' a of
        Just (L1 f'') -> Just f''
        _ -> Nothing

      matchR :: (a -> Maybe ((f :+: g) q)) -> a -> Maybe (g q)
      matchR _g' a = case _g' a of
        Just (R1 g'') -> Just g''
        _ -> Nothing


instance MkStackPrism f => MkStackPrismList (M1 C c f) where

  data StackPrismList (M1 C c f) a = I (forall t. StackPrism (StackPrismLhs f t) (a :- t))

  mkStackPrismList' f' g' = I (stackPrism (f f') (g g'))
    where
      f :: forall a p t. (M1 C c f p -> a) -> StackPrismLhs f t -> a :- t
      f _f' lhs = mapHead (_f' . M1) (mkR lhs)
      g :: forall a p t. (a -> Maybe (M1 C c f p)) -> (a :- t) -> Maybe (StackPrismLhs f t)
      g _g' (a :- t) = fmap (mkL . (:- t) . unM1) (_g' a)


-- Deriving types and conversions for single constructors

class MkStackPrism (f :: * -> *) where
  type StackPrismLhs (f :: * -> *) (t :: *) :: *
  mkR :: forall p t. StackPrismLhs f t -> (f p :- t)
  mkL :: forall p t. (f p :- t) -> StackPrismLhs f t

instance MkStackPrism U1 where
  type StackPrismLhs U1 t = t
  mkR t         = U1 :- t
  mkL (U1 :- t) = t

instance MkStackPrism (K1 i a) where
  type StackPrismLhs (K1 i a) t = a :- t
  mkR (h :- t) = K1 h :- t
  mkL (K1 h :- t) = h :- t

instance MkStackPrism f => MkStackPrism (M1 i c f) where
  type StackPrismLhs (M1 i c f) t = StackPrismLhs f t
  mkR = mapHead M1 . mkR
  mkL = mkL . mapHead unM1

instance (MkStackPrism f, MkStackPrism g) => MkStackPrism (f :*: g) where
  type StackPrismLhs (f :*: g) t = StackPrismLhs f (StackPrismLhs g t)
  mkR t = (hf :*: hg) :- tg
    where
      hf :- tf = mkR t
      hg :- tg = mkR tf
  mkL ((hf :*: hg) :- t) = mkL (hf :- mkL (hg :- t))



mapHead :: (a -> b) -> (a :- t) -> (b :- t)
mapHead f (h :- t) = f h :- t
