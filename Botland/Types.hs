{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, FlexibleInstances, DeriveGeneric #-}

module Botland.Types where

import GHC.Generics
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero, guard)

import Data.Data (Data, Typeable, typeOf)
import Data.Aeson (ToJSON, (.=), object, (.:), FromJSON, Object(..), Value, parseJSON, toJSON)
import qualified Data.Aeson as A
import Data.Aeson.Types (Parser, Pair)

import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString) 
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.Char (toLower, toUpper) 

import Data.Map (Map)
import Data.Maybe (isJust, fromJust)
import Data.List.Split (splitOn)
import qualified Data.Vector as V (toList)

import Safe (readMay, readDef)



type Id = String
type UnitToken = String


-- Point, just an X, Y
data Point = Point { x :: Int, y :: Int } deriving (Generic)

showPoint :: Point -> ByteString
showPoint p = B.pack $ (show (x p)) ++ "." ++ (show (y p))

readPoint :: ByteString -> Point
readPoint cs = Point (read x) (read y)
    where xy = splitOn "." (B.unpack cs)
          x = xy !! 0
          y = xy !! 1 

instance ToJSON Point
instance FromJSON Point

data Unit a = Unit { id :: Id, token :: UnitToken, obj :: a }
            deriving (Generic)

instance FromJSON Unit
instance ToJSON Unit

type Unit = (Id, UnitToken, Actor)

type Field = Map Point Id  -- yeah how it's stored in the system, sure.

data BlockType = Wood | Stone deriving (Generic, Typeable, Show)
data ActorType = Bot | Player deriving (Generic, Typeable, Show, Read)

-- a rectangle!
data Bounds = Bounds 

-- ! can't access unless Point is Ord
-- the field could contain blocks too, no? 
-- either way, it makes sense for it to contain strings... 
-- but it would be far better if it contained blocks too

type Location = (Point, Id)
data Field = Field [Location] deriving (Generic)
instance ToJSON Field where
    toJSON (Field fs) = object $ map jsonHashPoint fs
        where jsonHashPoint (p, i) = (decodeUtf8 $ showPoint p) .= toJSON i

toLocation :: (ByteString, ByteString) -> Location
toLocation (l, v) = ((readPoint l), (B.unpack v))

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
    --toJSON (Unit i o) = object $ idObjectToJSON i o
    toJSON (Unit i t o) = object $ ["token" .= t] ++ idObjectToJSON i o

idObjectToJSON :: (ToJSON a, Typeable a) => Id -> a -> [Pair]
idObjectToJSON i o = ["id" .= i, typePropertyName o .= toJSON o]
    

-- message you might want to return. I used data because it's easier to serialize / parse
data Fault = Fault { message :: String } 
           | NotFound
           | NotAuthorized
           deriving (Generic)
instance FromJSON Fault
instance ToJSON Fault

data Empty = Empty deriving (Generic)
instance ToJSON Empty





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




