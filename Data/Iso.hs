{-# LANGUAGE TypeOperators #-}

module Data.Iso (

  -- * Partial isomorphisms
  Iso(..), forward, backward,
  FromIso(..),
  (:-)(..)

  ) where


import Prelude hiding (id, (.), head)

import Data.Semigroup (Semigroup(..))

import Control.Applicative hiding (many)
import Control.Monad
import Control.Category


-- | Bidirectional isomorphism that is partial in the backward direction.
data Iso a b = Iso (a -> b) (b -> Maybe a)

instance Category Iso where
  id                          = Iso id Just
  ~(Iso f1 g1) . ~(Iso f2 g2) = Iso (f1 . f2) (g1 >=> g2)

instance Semigroup (Iso a b) where
  ~(Iso f1 g1) <> ~(Iso f2 g2) =
    Iso
      f1
      ((<|>) <$> g1 <*> g2)

-- | Apply an isomorphism in forward direction.
forward :: Iso a b -> a -> b
forward (Iso f _) = f

-- | Apply an isomorphism in backward direction.
backward :: Iso a b -> b -> Maybe a
backward (Iso _ g) = g

class Category cat => FromIso cat where
  fromIso :: Iso a b -> cat a b

instance FromIso Iso where
  fromIso = id

-- | Heterogenous stack with a head and a tail.
data h :- t = h :- t
  deriving (Eq, Show)
infixr 5 :-
