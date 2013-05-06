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


person :: Iso (String :- Gender :- Int :- Coords :- t) (Person :- t)
male   :: Iso t (Gender :- t)
female :: Iso t (Gender :- t)
coords :: Iso (Float :- Float :- t) (Coords :- t)

(person, male, female, coords) =
    (person', male', female', coords')
  where
    IsoList (Z person')            = mkIsoList
    IsoList (Z male' :& Z female') = mkIsoList
    IsoList (Z coords')            = mkIsoList
