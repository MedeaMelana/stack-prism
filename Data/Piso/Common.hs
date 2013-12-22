{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonoPatBinds #-}

-- | Constructor-destructor isomorphisms for some common datatypes.
module Data.Piso.Common (

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

import Data.Piso
import Data.Piso.TH


unit :: Piso t (() :- t)
unit = Piso f g
  where
    f       t  = () :- t
    g (_ :- t) = Just t

tup :: Piso (a :- b :- t) ((a, b) :- t)
tup = Piso f g
  where
    f (a :- b :- t) = (a, b) :- t
    g ((a, b) :- t) = Just (a :- b :- t)

tup3 :: Piso (a :- b :- c :- t) ((a, b, c) :- t)
tup3 = Piso f g
  where
    f (a :- b :- c :- t) = (a, b, c) :- t
    g ((a, b, c) :- t) = Just (a :- b :- c :- t)

nothing :: Piso t (Maybe a :- t)
just    :: Piso (a :- t) (Maybe a :- t)
(nothing, just) = $(derivePisos ''Maybe)


nil :: Piso t ([a] :- t)
nil = Piso f g
  where
    f        t  = [] :- t
    g ([] :- t) = Just t
    g _         = Nothing

cons :: Piso (a :- [a] :- t) ([a] :- t)
cons = Piso f g
  where
    f (x :- xs  :- t) = (x : xs) :- t
    g ((x : xs) :- t) = Just (x :- xs :- t)
    g _               = Nothing


left  :: Piso (a :- t) (Either a b :- t)
right :: Piso (b :- t) (Either a b :- t)
(left, right) = $(derivePisos ''Either)


false :: Piso t (Bool :- t)
true  :: Piso t (Bool :- t)
(false, true) = $(derivePisos ''Bool)
