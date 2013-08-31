{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonoPatBinds #-}

-- | Constructor-destructor isomorphisms for some common datatypes.
module Data.Iso.Common (

  -- * @()@
  unit,
  
  -- * @(,)@
  tup,
  
  -- * @(,,)@
  tup3,

  -- * @Maybe a@
  nothing, just,
  
  -- * @[a]@
  nil, cons,
  
  -- * @Either a b@
  left, right,
  
  -- * @Bool@
  false, true

  ) where

import Prelude hiding (id, (.), maybe, either)

import Data.Iso.Core
import Data.Iso.TH


unit :: Iso t (() :- t)
unit = Iso f g
  where
    f       t  = () :- t
    g (_ :- t) = Just t

tup :: Iso (a :- b :- t) ((a, b) :- t)
tup = Iso f g
  where
    f (a :- b :- t) = (a, b) :- t
    g ((a, b) :- t) = Just (a :- b :- t)

tup3 :: Iso (a :- b :- c :- t) ((a, b, c) :- t)
tup3 = Iso f g
  where
    f (a :- b :- c :- t) = (a, b, c) :- t
    g ((a, b, c) :- t) = Just (a :- b :- c :- t)

nothing :: Iso t (Maybe a :- t)
just    :: Iso (a :- t) (Maybe a :- t)
(nothing, just) = $(deriveIsos ''Maybe)


nil :: Iso t ([a] :- t)
nil = Iso f g
  where
    f        t  = [] :- t
    g ([] :- t) = Just t
    g _         = Nothing

cons :: Iso (a :- [a] :- t) ([a] :- t)
cons = Iso f g
  where
    f (x :- xs  :- t) = (x : xs) :- t
    g ((x : xs) :- t) = Just (x :- xs :- t)
    g _               = Nothing


left  :: Iso (a :- t) (Either a b :- t)
right :: Iso (b :- t) (Either a b :- t)
(left, right) = $(deriveIsos ''Either)


false :: Iso t (Bool :- t)
true  :: Iso t (Bool :- t)
(false, true) = $(deriveIsos ''Bool)
