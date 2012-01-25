{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, FlexibleInstances, DeriveGeneric #-}

module Botland.Types where

import GHC.Generics
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero, guard)

import Data.Data (Data, Typeable, typeOf)
import Data.Aeson (ToJSON, (.=), object, (.:), FromJSON, Object(..), Value, parseJSON, toJSON)
import qualified Data.Aeson as A
import Data.Aeson.Types (Parser)

import Data.Text (Text, pack, unpack)
import Data.Char (toLower, toUpper) 

import Data.Map (Map)
import Data.Maybe (isJust, fromJust)

import Safe (readMay, readDef)



type Id = String
data Point = Point { x :: Int, y :: Int } deriving (Show, Generic)
instance ToJSON Point
instance FromJSON Point

data Unit = CreatureUnit Id Creature
          | BlockUnit Id Object
          deriving (Generic)

data BlockType = Wood | Stone deriving (Generic, Typeable, Show)
data CreatureType = Bot | Player deriving (Generic, Typeable, Show, Read)

newtype Field = Field (Map Point Unit) deriving (Generic)

data Creature = Creature CreatureType deriving (Typeable, Show)
data Block = Block BlockType deriving (Generic, Typeable)

instance FromJSON Creature where
    parseJSON (A.Object v) = do
        mt <- readMay <$> (v .: "type") 
        guard (isJust mt)
        return $ Creature $ fromJust mt
    parseJSON _          = mzero

instance ToJSON Creature where
    -- really, what I probably want is a function that can include the "type" part and concat my list with another
    toJSON (Creature t) = object ["type" .= t]

instance FromJSON CreatureType
instance ToJSON CreatureType 
    where toJSON = typeJSON

instance FromJSON BlockType
instance ToJSON BlockType where toJSON = typeJSON

instance FromJSON Unit
instance ToJSON Unit where
    toJSON (CreatureUnit i c) = idObjectToJSON i c
    toJSON (BlockUnit i o) = idObjectToJSON i o -- just use a standard function for now

idObjectToJSON :: (ToJSON a, Typeable a) => Id -> a -> Value
idObjectToJSON i o = object ["id" .= i, typePropertyName o .= toJSON o]
    

-- message you might want to return. I used data because it's easier to serialize / parse
data Error = Error { error :: String } deriving (Generic)
instance FromJSON Error
instance ToJSON Error





-- instance (Data a, Typeable a) => ToJSON (Message a) where
--     toJSON (Message o) = object [messagePair o]
--     toJSON (Token o t) = object [messagePair o, "token" .= t]
-- 
-- helpers for serializing messages
-- messagePair :: (Data a, Typeable a) => a -> Pair
-- messagePair a = typeName a .= AG.toJSON a

-- returns the lowercase typename for a Typeable a
typePropertyName :: (Typeable a) => a -> Text
typePropertyName = pack . map toLower . show . typeOf

-- don't lowercase these guys
typeJSON :: (Show a) => a -> Value
typeJSON = A.String . pack . show




