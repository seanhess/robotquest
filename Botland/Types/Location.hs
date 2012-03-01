{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, DeriveGeneric #-}

module Botland.Types.Location where

import Data.ByteString.Char8 (ByteString)
import Data.Data (Data, Typeable, typeOf)

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

data Point = Point { x :: Int, y :: Int } deriving (Generic, Show)
data Size = Size { width :: Int, height :: Int } deriving (Generic, Show)
data Field = Field { fieldStart :: Point, fieldSize :: Size, locations :: [Location] } deriving (Generic, Show)

-- change locations to be a simple array of location objects
data Location = Location { point :: Point, unitId :: ByteString } deriving (Generic, Show)


instance ToJSON Point
instance FromJSON Point

instance ToJSON Size
instance FromJSON Size

instance ToJSON Field
instance FromJSON Field

instance ToJSON Location
instance FromJSON Location







