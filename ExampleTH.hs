{-# LANGUAGE TemplateHaskell #-}

module Example where

import Data.StackPrism
import Data.StackPrism.TH


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

deriveStackPrisms ''Person
deriveStackPrisms ''Gender
deriveStackPrisms ''Coords