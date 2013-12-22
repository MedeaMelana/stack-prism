{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}

module Data.Piso (

  -- * Partial isomorphisms
  Piso(..), forward, backward,
  FromPiso(..),
  (:-)(..)

  ) where


import Prelude hiding (id, (.))

import Control.Monad ((>=>))
import Control.Category (Category(..))


-- | Bidirectional isomorphism that is partial in the backward direction.
data Piso a b = Piso (a -> b) (b -> Maybe a)

instance Category Piso where
  id                          = Piso id Just
  ~(Piso f1 g1) . ~(Piso f2 g2) = Piso (f1 . f2) (g1 >=> g2)


-- | Apply an isomorphism in forward direction.
forward :: Piso a b -> a -> b
forward (Piso f _) = f

-- | Apply an isomorphism in backward direction.
backward :: Piso a b -> b -> Maybe a
backward (Piso _ g) = g


-- | A type class that expresses that a category is able to embed 'Piso' values.
class Category cat => FromPiso cat where
  fromPiso :: Piso a b -> cat a b

instance FromPiso Piso where
  fromPiso = id


-- | Heterogenous stack with a head and a tail. Or: an infix way to write @(,)@.
data h :- t = h :- t
  deriving (Eq, Show, Functor)
infixr 5 :-
