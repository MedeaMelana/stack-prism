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


$(derivePisos ''() ["unit"])
$(derivePisos ''(,) ["tup"])
$(derivePisos ''(,,) ["tup3"])

$(derivePisos ''Maybe ["nothing", "just"])

nil :: Piso t ([a] :- t)
nil = piso f g
  where
    f        t  = [] :- t
    g ([] :- t) = Just t
    g _         = Nothing

cons :: Piso (a :- [a] :- t) ([a] :- t)
cons = piso f g
  where
    f (x :- xs  :- t) = (x : xs) :- t
    g ((x : xs) :- t) = Just (x :- xs :- t)
    g _               = Nothing

$(derivePisos ''Either ["left", "right"])

$(derivePisos ''Bool ["false", "true"])
