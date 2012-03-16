{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, DeriveGeneric #-}

module Botland.Types.Unit where

import Botland.Types.Location (Point(..))

import Data.ByteString.Char8 (ByteString)
import Data.Data (Data, Typeable, typeOf)
import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Data.Text as T 


import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero, guard)
import Data.Maybe (fromJust, isJust, fromMaybe)
import Safe (readMay)

import GHC.Generics (Generic)

type UnitId = ByteString
type UnitToken = ByteString

-- they already know their description
data SpawnRequest = SpawnRequest { requestedPoint :: Point, unitDescription :: UnitDescription } deriving (Generic, Show)
instance FromJSON SpawnRequest
instance ToJSON SpawnRequest

data Spawn = Spawn { unitId :: UnitId, unitToken :: UnitToken, spawnPoint :: Point } deriving (Generic, Show)
instance ToJSON Spawn
instance FromJSON Spawn

data Block = Block { blockType :: BlockType } deriving (Data, Typeable, Show, Generic)
data BlockType = Wood | Stone deriving (Data, Typeable, Show, Read, Generic)

instance FromJSON Block
instance ToJSON Block

instance ToJSON BlockType where toJSON = typeToJSON
instance FromJSON BlockType where parseJSON = typeParseJSON

data UnitDescription = UnitDescription { source :: ByteString, name :: ByteString, kind :: ByteString, notes :: ByteString, color :: ByteString } deriving (Data, Typeable, Show, Generic)

instance FromJSON UnitDescription
instance ToJSON UnitDescription



















typeToJSON :: (Show a) => a -> A.Value
typeToJSON = A.String . T.pack . show

typeParseJSON :: (Read a) => A.Value -> AT.Parser a
typeParseJSON (A.String t) = do
    let mt = readMay $ T.unpack t
    guard (isJust mt)
    return $ fromJust mt
typeParseJSON _ = mzero 
