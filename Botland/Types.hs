{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Botland.Types where

import Data.Aeson (FromJSON(..), ToJSON(..), object, (.=), (.:), Value(..))

import Control.Applicative ((<$>), (<*>))

import GHC.Generics (Generic)

-- this is roughly what it looks like in the database
--{x, y, _id, color, mcpId, name, source}
data Bot = Bot { id :: String
               , x :: Int
               , y :: Int
               , name :: String
               , source :: String
               , mcpId :: String
               , color :: String
               } deriving (Show)

-- gives the field and interval
data Game = Game { width :: Int
                 , height :: Int
                 , tick :: Int 
                 } deriving (Show, Generic)

instance ToJSON Game
instance FromJSON Game


-- custom serialization baby!!!