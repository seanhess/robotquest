{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, DeriveGeneric #-}

module Botland.Types.Unit where

import Data.ByteString.Char8 (ByteString)
import Data.Data (Data, Typeable, typeOf)
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)

type UnitId = ByteString
type UnitToken = ByteString
data Unit = Unit { unitId :: UnitId 
                  , unitToken :: UnitToken 
                  , unitDescription :: UnitDescription
                  } deriving (Generic, Typeable, Show)

instance FromJSON Unit
instance ToJSON Unit



data Block = Block { blockType :: BlockType } deriving (Data, Typeable, Show, Generic)
data BlockType = Wood | Stone deriving (Data, Typeable, Show, Generic)

instance FromJSON Block
instance ToJSON Block

instance FromJSON BlockType where
    

instance ToJSON BlockType

data UnitDescription = UnitDescription { source :: ByteString, notes :: ByteString } deriving (Data, Typeable, Show, Generic)

instance FromJSON UnitDescription
instance ToJSON UnitDescription