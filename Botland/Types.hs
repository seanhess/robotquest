{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, FlexibleInstances, DeriveGeneric #-}

module Botland.Types where

import Control.Applicative ((<$>))

import Data.Data (Data, Typeable, typeOf)
import Data.Aeson (ToJSON(..), (.=), object, (.:), FromJSON, Object(..))
import Data.Aeson.Types (Pair)

import qualified Data.Aeson.Generic as AG
import qualified Data.Text as T
import qualified Data.Aeson as A

import Data.Char (toLower)

data Unit = Unit { x :: Int, y :: Int } deriving (Show, Data, Typeable)





-- ok, so, basically, my responses are :
-- {<type>:{...}}
-- {error: ""}
-- {token:"", <type>:{...}}

-- I couldn't get Error to be a value constructor of Message for some reason
data Message a = Message { obj :: a } |
                 Token { obj :: a , token :: String } 
                 deriving (Show, Data, Typeable)

instance (Data a, Typeable a) => ToJSON (Message a) where
    toJSON (Message o) = object [messagePair o]
    toJSON (Token o t) = object [messagePair o, "token" .= t]

-- the decode :: Message Unit tells it which kind to use, no? Token Unit? Is that a type?
-- not sure I know. Try just parsing it straight up
-- instance (Data a, Typeable a) => FromJSON (Message a) where
--     parseJSON (A.Object o) = Message <$> unit
--         where unit = AG.fromJSON (o .: "unit") :: Unit

-- instance FromJSON (Message Unit) where
--     parseJSON (A.Object o) = Message <$> (Unit 10 10)

newtype Error = Error { errMessage :: String }
instance ToJSON Error where
    toJSON s = object ["error" .= errMessage s]

-- helpers for serializing messages
messagePair :: (Data a, Typeable a) => a -> Pair
messagePair a = typeName a .= AG.toJSON a

-- returns the lowercase typename for a Typeable a
typeName :: (Typeable a) => a -> T.Text
typeName a = T.pack $ map toLower $ show $ typeOf a

