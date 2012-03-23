{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Botland.Types where

import Data.Aeson (FromJSON(..), ToJSON(..), object, (.=), (.:), Value(..))
import Data.Aeson.Types (Parser)

import Database.MongoDB (val, Document, Field(..), at, lookup)

import Data.Maybe (fromMaybe)

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)

import GHC.Generics (Generic)

import Prelude hiding (lookup)

-- this is roughly what it looks like in the database
--{x, y, _id, color, mcpId, name, source}
data Bot = Bot { x :: Int
               , y :: Int
               , name :: String
               , source :: String
               , color :: String
               , botId :: Maybe String
               , mcpId :: Maybe String
               } deriving (Show)

-- gives the field and interval
data Game = Game { width :: Int
                 , height :: Int
                 , tick :: Int 
                 } deriving (Show, Generic)

instance ToJSON Game
instance FromJSON Game


-- when I just want to send back an id
data Id = Id { id :: String } deriving (Show, Generic)

instance ToJSON Id
instance FromJSON Id



-- for locations call
instance ToJSON Bot where
    toJSON b = object fs 
        where id = botId b
              fs = [ "id" .= id 
                   , "x" .= x b
                   , "y" .= y b
                   , "name" .= name b
                   , "source" .= source b
                   , "color" .= color b 
                   ]

-- for new bot call
instance FromJSON Bot where 
    parseJSON (Object v) = do
        x <- v .: "x"
        y <- v .: "y" 
        name <- v .: "name"
        source <- v .: "source"
        color <- v .: "color"
        return $ Bot x y name source color Nothing Nothing 

    parseJSON _ = mzero





-- bson mapping

toDoc :: Bot -> Document
toDoc b = [ "x" := val (x b)
          , "y" := val (y b)
          , "name" := val (name b)
          , "source" := val (source b)
          , "color" := val (color b)
          , "_id" := val (fromMaybe "" (botId b))
          , "mcpId" := val (fromMaybe "" (mcpId b))
          ]


fromDoc :: Document -> Bot
fromDoc d = Bot (at "x" d) (at "y" d) (at "name" d) (at "source" d) (at "color" d) (lookup "_id" d) (lookup "mcpId" d) 



fakeBot :: Bot
fakeBot = Bot 1 1 "name" "source" "#000" (Just "woot") (Just "mcpId")


-- custom serialization baby!!!