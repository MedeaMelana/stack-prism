{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}

module Data.Piso (

  -- * Partial isomorphisms
  Piso, piso, forward, backward, 
  (:-)(..)

  ) where


import Control.Applicative
import Data.Profunctor (Choice(..))
import Data.Profunctor.Unsafe
import Data.Functor.Identity
import Data.Monoid (First(..))
import Data.Tagged

-- | Bidirectional isomorphism that is partial in the backward direction.
--
-- This can be used to express constructor-deconstructor pairs. For example:
--
-- > nil :: Piso t ([a] :- t)
-- > nil = piso f g
-- >   where
-- >     f        t  = [] :- t
-- >     g ([] :- t) = Just t
-- >     g _         = Nothing
-- >
-- > cons :: Piso (a :- [a] :- t) ([a] :- t)
-- > cons = piso f g
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

type Piso a b = forall p f. (Choice p, Applicative f) => p a (f a) -> p b (f b)

piso :: (a -> b) -> (b -> Maybe a) -> Piso a b
piso f g = dimap (\b -> maybe (Left b) Right (g b)) (either pure (fmap f)) . right'

-- | Apply an isomorphism in forward direction.
forward :: Piso a b -> a -> b
forward l = runIdentity #. unTagged #. l .# Tagged .# Identity

-- | Apply an isomorphism in backward direction.
backward :: Piso a b -> b -> Maybe a
backward l = getFirst #. getConst #. l (Const #. First #. Just)


-- | Heterogenous stack with a head and a tail. Or: an infix way to write @(,)@.
data h :- t = h :- t
  deriving (Eq, Show, Functor)
infixr 5 :-
