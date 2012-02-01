{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, FlexibleInstances, DeriveGeneric #-}

module Botland.Types where

import GHC.Generics
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero, guard)

import Data.Data (Data, Typeable, typeOf)
import Data.Aeson (ToJSON, (.=), object, (.:), FromJSON, Object(..), Value, parseJSON, toJSON)
import qualified Data.Aeson as A
import Data.Aeson.Types (Parser, Pair)

import Data.Text (Text, pack, unpack)
import Data.Char (toLower, toUpper) 

import Data.Map (Map)
import Data.Maybe (isJust, fromJust)
import Data.List.Split (splitOn)

import Safe (readMay, readDef)



type Id = String
type UnitToken = String

data Point = Point { x :: Int, y :: Int } deriving (Generic)

-- wasn't worth implementing read
showPoint :: Point -> String
showPoint p = (show (x p)) ++ "." ++ (show (y p))

readPoint :: String -> Point
readPoint cs = Point (read x) (read y)  --- can't do it this way! Might have multiple digits!
    where xy = splitOn "." cs 
          x = xy !! 0
          y = xy !! 1 

instance ToJSON Point
instance FromJSON Point

-- what about when I want to represent it with the token
-- it's more an association of an actor with an id

-- unit means anything with an id, or an id and token
data Unit a = Unit { iden :: String, obj :: a }
            | Token { iden :: String, token :: String, obj :: a }
            deriving (Generic)
-- can't decide if it should be all the same thing or not

data BlockType = Wood | Stone deriving (Generic, Typeable, Show)
data ActorType = Bot | Player deriving (Generic, Typeable, Show, Read)

-- ! can't access unless Point is Ord
data Field = Field [(Point, Id)] deriving (Generic)
instance ToJSON Field where
    toJSON (Field fs) = object $ map jsonHashPoint fs
        where jsonHashPoint (p, i) = (pack $ showPoint p) .= toJSON i

data Actor = Actor ActorType deriving (Typeable, Show)

instance FromJSON Actor where
    parseJSON (A.Object v) = do
        mt <- readMay <$> (v .: "type") 
        guard (isJust mt)
        return $ Actor $ fromJust mt
    parseJSON _          = mzero

instance ToJSON Actor where
    -- really, what I probably want is a function that can include the "type" part and concat my list with another
    toJSON (Actor t) = object ["type" .= t]

instance FromJSON ActorType
instance ToJSON ActorType where toJSON = typeJSON

instance FromJSON BlockType
instance ToJSON BlockType where toJSON = typeJSON

instance (FromJSON a) => FromJSON (Unit a)
instance (ToJSON a, Typeable a) => ToJSON (Unit a) where
    toJSON (Unit i o) = object $ idObjectToJSON i o
    toJSON (Token i t o) = object $ ["token" .= t] ++ idObjectToJSON i o

idObjectToJSON :: (ToJSON a, Typeable a) => Id -> a -> [Pair]
idObjectToJSON i o = ["id" .= i, typePropertyName o .= toJSON o]
    

-- message you might want to return. I used data because it's easier to serialize / parse
data Fault = Fault { message :: String } 
           | NotFound
           deriving (Generic)
instance FromJSON Fault
instance ToJSON Fault





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




