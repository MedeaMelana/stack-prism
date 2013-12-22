{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoMonoPatBinds #-}

module Example where

import Data.Piso
import Data.Piso.TH


data Person = Person
  { name     :: String
  , gender   :: Gender
  , age      :: Int
  , location :: Coords
  } deriving (Eq, Show)

data Gender = Male | Female
  deriving (Eq, Show)

data Coords = Coords { lat :: Float, lng :: Float }
  deriving (Eq, Show)

person :: Piso (String :- Gender :- Int :- Coords :- t) (Person :- t)
person = $(derivePisos ''Person)

male   :: Piso t (Gender :- t)
female :: Piso t (Gender :- t)
(male, female) = $(derivePisos ''Gender)

coords :: Piso (Float :- Float :- t) (Coords :- t)
coords = $(derivePisos ''Coords)
