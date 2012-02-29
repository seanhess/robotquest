{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, DeriveGeneric #-}

module Botland.Types.Message where

import Data.ByteString.Char8 (ByteString)
import Data.Data (Data, Typeable)

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON, object, (.=))

-- message you might want to return.
data Fault = Fault { message :: ByteString } 
           | NotFound
           | NotAuthorized
           deriving (Generic, Show)

instance FromJSON Fault
instance ToJSON Fault



data Empty = Empty deriving (Generic)

instance FromJSON Empty
instance ToJSON Empty

