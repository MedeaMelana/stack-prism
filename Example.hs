{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonoPatBinds #-}

module Example where

import Data.Iso

import Prelude hiding (id, (.), head, either)


data Person = Person
  { name     :: String
  , gender   :: Gender
  , age      :: Int
  -- , lat      :: Float
  -- , lng      :: Float
  , location :: Coords
  } deriving (Eq, Show)

data Gender = Male | Female
  deriving (Eq, Show)

data Coords = Coords { lat :: Float, lng :: Float }
  deriving (Eq, Show)

person         = $(deriveIsos ''Person)
(male, female) = $(deriveIsos ''Gender)
coords         = $(deriveIsos ''Coords)
