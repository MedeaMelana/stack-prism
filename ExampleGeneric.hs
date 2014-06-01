{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Example where

import Data.StackPrism
import Data.StackPrism.Generic

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
person :: StackPrism (String :- Gender :- Int :- Coords :- t) (Person :- t)
male   :: StackPrism t (Gender :- t)
female :: StackPrism t (Gender :- t)
coords :: StackPrism (Float :- Float :- t) (Coords :- t)

PrismList (P person)           = mkPrismList :: StackPrisms Person
PrismList (P male :& P female) = mkPrismList :: StackPrisms Gender
PrismList (P coords)           = mkPrismList :: StackPrisms Coords


false, true :: StackPrism t (Bool :- t)
PrismList (P false :& P true) = mkPrismList :: StackPrisms Bool

nil  :: StackPrism              t  ([a] :- t)
cons :: StackPrism (a :- [a] :- t) ([a] :- t)
PrismList (P nil :& P cons) = mkPrismList :: StackPrisms [a] 
