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
--
-- This can be used to express constructor-deconstructor pairs. For example:
--
-- > nil :: Piso t ([a] :- t)
-- > nil = Piso f g
-- >   where
-- >     f        t  = [] :- t
-- >     g ([] :- t) = Just t
-- >     g _         = Nothing
-- >
-- > cons :: Piso (a :- [a] :- t) ([a] :- t)
-- > cons = Piso f g
-- >   where
-- >     f (x :- xs  :- t) = (x : xs) :- t
-- >     g ((x : xs) :- t) = Just (x :- xs :- t)
-- >     g _               = Nothing
--
-- Here ':-' can be read as \'cons\', forming a stack of values. For example,
-- @nil@ pushes @[]@ onto the stack; or, in the backward direction, tries to
-- remove @[]@ from the stack. Representing constructor-destructor pairs as
-- stack manipulators allows them to be composed more easily.
--
-- Module @Data.Piso.Common@ contains @Piso@s for some common datatypes.
--
-- Modules @Data.Piso.Generic@ and @Data.Piso.TH@ offer generic ways of deriving @Piso@s for custom datatypes.

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
