{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, DeriveGeneric #-}

module Botland.Types.Message where

import Data.ByteString.Char8 (ByteString)
import Data.Data (Data, Typeable)

import GHC.Generics (Generic)
import Data.Aeson (FromJSON(..), ToJSON(..), object, (.=), (.:), Value(..))

import Control.Applicative ((<$>), (<*>))

-- message you might want to return.
data Fault = Fault ByteString
           | NotFound
           | NotAuthorized
           deriving (Generic, Show)

instance FromJSON Fault where
    parseJSON (Object v) = Fault <$> v .: "message"

instance ToJSON Fault where
    toJSON f = object ["message" .= message f]

message :: Fault -> ByteString 
message NotFound = "Not Found"
message NotAuthorized = "Not Authorized"
message (Fault m) = m

data Empty = Empty deriving (Generic)

instance FromJSON Empty
instance ToJSON Empty

data Test = Test { test :: ByteString } deriving (Generic, Data, Typeable, Show)
instance ToJSON Test
instance FromJSON Test
