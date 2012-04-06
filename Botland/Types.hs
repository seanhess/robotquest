{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Botland.Types where

import qualified Data.Aeson as A
import Data.Aeson (FromJSON(..), ToJSON(..), object, (.=), (.:), Value(..))
import qualified Data.Aeson.Types as AT
import Data.Aeson.Types (Parser)
import Data.Maybe (fromMaybe, isJust, fromJust)
import qualified Data.Text as T
import Data.DateTime (DateTime, fromSeconds)

import Database.MongoDB (val, Document, Field(..), at, lookup)

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero, guard)

import GHC.Generics (Generic)

import Prelude hiding (lookup)

import Safe (readMay)



-- DATA TYPES ---------------------------------------------------------------

-- heartbeat doesn't need to be a part of the client-side logic, just in the db
data Player = Player { playerId :: String, playerName :: String, source :: String } deriving (Show)

-- this is roughly what it looks like in the database
-- {x, y, _id, color, playerId, name, source}
data Bot = Bot { x :: Int
               , y :: Int
               , name :: String
               , player :: String
               , sprite :: String
               , botId :: String
               , botPlayerId :: String
               , kills :: Int
               , created :: DateTime
               } deriving (Show)

-- sometimes you just need to talk about a point
data Point = Point { px :: Int, py :: Int } deriving (Show)

-- gives the field and interval
data Game = Game { width :: Int
                 , height :: Int
                 , tick :: Int 
                 } deriving (Show, Generic)


-- available actions
data BotCommand = BotCommand { action :: BotAction, direction :: Direction } deriving (Show, Generic)
data Direction = DLeft | DRight | DUp | DDown deriving (Show, Read)
data BotAction = Stop | Move | Attack deriving (Show, Read)

-- things that can go wrong
data Fault = Fault String
           | NotFound
           | NotAuthorized
           | NotImplemented
           | InvalidPosition
           deriving (Generic, Show)

-- when I just want to send back an id
data Id = Id { id :: String } deriving (Show, Generic)

-- just means the server was successful
data Ok = Ok deriving (Show)






-- JSON SUPPORT --------------------------------------------------------------

instance ToJSON Game
instance FromJSON Game

instance ToJSON Id
instance FromJSON Id

instance ToJSON BotCommand
instance FromJSON BotCommand 

instance ToJSON Ok where
    toJSON _ = object ["ok" .= True]

-- Actions --
instance ToJSON BotAction where
    toJSON = typeToJSON show

instance FromJSON BotAction where
    parseJSON = typeParseJSON readMay

instance ToJSON Direction where
    toJSON = typeToJSON (removeFirstLetter.show)

instance FromJSON Direction where
    parseJSON = typeParseJSON (readMay.addD)

removeFirstLetter = tail
addD cs = 'D':cs

-- Bot --
instance ToJSON Bot where
    toJSON b = object fs 
        where id = botId b
              fs = [ "id" .= id 
                   , "x" .= x b
                   , "y" .= y b
                   , "name" .= name b
                   , "player" .= player b
                   , "sprite" .= sprite b 
                   , "kills" .= kills b
                   , "created" .= created b
                   ]

instance FromJSON Bot where 
    parseJSON (Object v) = do
        x <- v .: "x"
        y <- v .: "y" 
        name <- v .: "name"
        sprite <- v .: "sprite"
        return $ Bot x y name "" sprite "" "" 0 (fromSeconds 0)
        -- you don't have read the action, PlayerId or id from the client, ever.

    parseJSON _ = mzero


-- Player: json only has a name (not the id) --
instance FromJSON Player where
    parseJSON (Object v) = do
        name <- v .: "name"
        source <- v .: "source"
        return $ Player "" name source
    parseJSON _ = mzero

instance ToJSON Player where
    toJSON (Player id name source) = object ["name" .= name, "source" .= source]


-- SERVER MESSAGES ----------------------------------------------------------

instance FromJSON Fault where
    parseJSON (Object v) = Fault <$> v .: "message"

instance ToJSON Fault where
    toJSON f = object ["message" .= message f]

message :: Fault -> String 
message NotFound = "Not Found"
message NotAuthorized = "Not Authorized"
message NotImplemented = "Not Implemented"
message InvalidPosition = "Invalid Position"
message (Fault m) = m




-- MONGODB -----------------------------------------------------------------

class ToDoc a where
    toDoc :: a -> Document

class FromDoc a where
    fromDoc :: Document -> a

instance ToDoc Bot where
    toDoc b = [ "x" := val (x b)
              , "y" := val (y b)
              , "name" := val (name b)
              , "player" := val (player b)
              , "sprite" := val (sprite b)
              , "_id" := val (botId b)
              , "playerId" := val (botPlayerId b)
              , "created" := val (created b)
              ]

instance FromDoc Bot where
    fromDoc d = Bot (at "x" d) 
                    (at "y" d) 
                    (at "name" d) 
                    (at "player" d) 
                    (at "sprite" d)
                    (fromMaybe "" (lookup "_id" d))
                    (fromMaybe "" (lookup "playerId" d))
                    (fromMaybe 0 (lookup "kills" d))
                    (fromMaybe (fromSeconds 0) (lookup "created" d))


instance FromDoc Point where
    fromDoc p = Point (at "x" p) (at "y" p)

instance FromDoc Player where
    fromDoc p = Player (fromMaybe "" (lookup "_id" p)) (at "name" p) (at "source" p)

instance ToDoc Player where
    toDoc p = [ "_id" := val (playerId p)
              , "name" := val (playerName p)
              , "source" := val (source p)
              ]



-- JSON HELPERS ---------------------------------------------------------------

typeToJSON :: (a -> String) -> a -> A.Value
typeToJSON show = A.String . T.pack . show

typeParseJSON :: (String -> Maybe a) -> A.Value -> AT.Parser a
typeParseJSON read (A.String t) = do
    let ma = read $ T.unpack t
    case ma of 
      Nothing -> mzero 
      Just a -> return a
typeParseJSON _ _ = mzero

--typeParseJSON (A.String t) = do
--    let mt = readMay $ T.unpack t
--    guard (isJust mt)
--    return $ fromJust mt
--typeParseJSON _ = mzero 

