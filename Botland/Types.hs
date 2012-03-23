{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Botland.Types where

import Data.Aeson (FromJSON(..), ToJSON(..), object, (.=), (.:), Value(..))

import Control.Applicative ((<$>), (<*>))

import GHC.Generics (Generic)

-- this is roughly what it looks like in the database
--{x, y, _id, color, mcpId, name, source}
data Bot = Bot { botId :: String
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



-- don't serialize the mcpId, because it is secret
instance ToJSON Bot where
    toJSON b = object fs 
        where fs = [ "id" .= botId b
                   , "x" .= x b
                   , "y" .= y b
                   , "name" .= name b
                   , "source" .= source b
                   , "color" .= color b 
                   ]


-- custom serialization baby!!!