{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Example where

import Data.Iso
import Data.Iso.Generic

import GHC.Generics


data Person = Person
  { name     :: String
  , gender   :: Gender
  , age      :: Int
  , location :: Coords
  } deriving (Eq, Show, Generic)

data Gender = Male | Female
  deriving (Eq, Show, Generic)

data Coords = Coords { lat :: Float, lng :: Float }
  deriving (Eq, Show, Generic)

-- The types in the first type parameter match those of the corresponding constructor's fields.
person :: Iso (String :- Gender :- Int :- Coords :- t) (Person :- t)
male   :: Iso t (Gender :- t)
female :: Iso t (Gender :- t)
coords :: Iso (Float :- Float :- t) (Coords :- t)

-- Derive all isomorphisms for the three datatypes above.
-- The weird indirection is necessary in GHC 7.6 because of
-- https://ghc.haskell.org/trac/ghc/ticket/7268
-- and should be fixed in GHC 7.8.
(person, male, female, coords) =
    (person', male', female', coords')
  where
    IsoList (I person')            = mkIsoList
    IsoList (I male' :& I female') = mkIsoList
    IsoList (I coords')            = mkIsoList


false, true :: Iso t (Bool :- t)
(false, true) = (false', true')
  where
    IsoList (I false' :& I true') = mkIsoList

nil  :: Iso              t  ([a] :- t)
cons :: Iso (a :- [a] :- t) ([a] :- t)
(nil, cons) = (nil', cons')
  where
    IsoList (I nil' :& I cons') = mkIsoList
