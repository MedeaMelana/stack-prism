{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Example where

import Data.Piso
import Data.Piso.Generic

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
person :: Piso (String :- Gender :- Int :- Coords :- t) (Person :- t)
male   :: Piso t (Gender :- t)
female :: Piso t (Gender :- t)
coords :: Piso (Float :- Float :- t) (Coords :- t)

PisoList (I person)           = mkPisoList :: Pisos Person
PisoList (I male :& I female) = mkPisoList :: Pisos Gender
PisoList (I coords)           = mkPisoList :: Pisos Coords


false, true :: Piso t (Bool :- t)
PisoList (I false :& I true) = mkPisoList :: Pisos Bool

nil  :: Piso              t  ([a] :- t)
cons :: Piso (a :- [a] :- t) ([a] :- t)
PisoList (I nil :& I cons) = mkPisoList :: Pisos [a] 
