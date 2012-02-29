{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module Botland.Types.Location where

import Data.ByteString.Char8 (ByteString)
import Data.Data (Data, Typeable, typeOf)

data Point = Point { x :: Int, y :: Int } deriving (Data, Typeable, Show)
data Size = Size { width :: Int, height :: Int } deriving (Data, Typeable, Show)
data Field = Field { fieldStart :: Point, fieldSize :: Size, locations :: [Location] } deriving (Data, Typeable, Show) 

-- change locations to be a simple array of location objects
data Location = Location { point :: Point, unitId :: ByteString } deriving (Data, Typeable, Show)


