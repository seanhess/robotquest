{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, FlexibleInstances, DeriveGeneric #-}

module Botland.Types where

import GHC.Generics
import Control.Applicative ((<$>), (<*>))

import Data.Data (Data, Typeable, typeOf)
import Data.Aeson (ToJSON(..), (.=), object, (.:), FromJSON, Object(..), parseJSON)
import Data.Aeson

-- interesting. this probably REQUIRES unitId to be set, no?
-- ok, well, that sucks. 
-- does unit have a locaiton, or does locaiton have a unit?
-- geez, this is so hard! I just want a DANG hash!
-- I might have all KINDS of properties like this What am I supposed to do?


-- so, the problem is that unitId is optional
-- oh geez, what a pain in the butt. 
-- I'm going to have to name all the fields unique things so they don't collide, 
-- but then I can re-use "type" or "id" or anything. Although type is reserved 

data Unit = Unit { unitId :: Maybe String, unitType :: String } deriving (Show, Generic)
instance ToJSON Unit
instance FromJSON Unit where
    parseJSON (Object o) = Unit <$> o .:? "unitId" -- if you have optional fields you'll have to write the dang parsers by hand
                                <*> o .: "type"

data Error = Error { error :: String } deriving (Generic)
instance FromJSON Error
instance ToJSON Error

-- ok, so, i need to figure out what my data model is. 
-- when they ask for a bunch of objects, I want to say that objects are x,y,z. 
-- but they don't have a location. they are AT a location
-- which is better for haskell, harder for the api. 
-- still, this API is for me. I can make the clients do whatever I want :)

data Location a = Location { x :: Int, y :: Int, unit :: a } deriving (Generic)
instance FromJSON a => FromJSON (Location a)
instance ToJSON a => ToJSON (Location a)

