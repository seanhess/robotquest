{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveDataTypeable #-}

module Botland.Types where

import qualified Data.Aeson as A
import Data.Aeson (FromJSON(..), ToJSON(..), object, (.=), (.:), Value(..))
import qualified Data.Aeson.Types as AT
import Data.Aeson.Types (Parser)
import Data.Bson (Val(..), Value(..))
import qualified Data.Bson as B
import qualified Data.CompactString.UTF8 as C
import Data.Maybe (fromMaybe, isJust, fromJust)
import qualified Data.Text as T
import Data.DateTime (DateTime, fromSeconds)
import Data.Map (Map)
import Data.Typeable (Typeable)

import Database.MongoDB (val, Document, Field(..), at, lookup)

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero, guard)

import GHC.Generics (Generic)

import Prelude hiding (lookup)

import Safe (readMay)


type Field = Map Point Bot



-- DATA TYPES ---------------------------------------------------------------

-- heartbeat doesn't need to be a part of the client-side logic, just in the db
data Player = Player { playerId :: String, playerName :: String, source :: String } deriving (Show)

-- this is roughly what it looks like in the database
-- {x, y, _id, color, playerId, name, source}
data Bot = Bot { point :: Point
               , name :: String
               , player :: String
               , sprite :: String
               , botId :: String
               , botPlayerId :: String
               , kills :: Int
               , created :: DateTime
               , botState :: BotState
               , command :: Maybe BotCommand
               } deriving (Show)

-- sometimes you just need to talk about a point
data Point = Point { x :: Int, y :: Int } 
           | InvalidPoint 
           deriving (Show, Ord, Eq)

-- gives the field and interval
data GameInfo = GameInfo { width :: Int
                 , height :: Int
                 , tick :: Integer
                 } deriving (Show, Generic)

-- available actions
data BotCommand = BotCommand { action :: BotAction, direction :: Direction } deriving (Show, Eq, Typeable, Generic)
data Direction = DLeft | DRight | DUp | DDown deriving (Show, Read, Eq, Typeable)
data BotAction = Stop | Move | Attack deriving (Show, Read, Eq, Typeable)
data BotState = Active | Dead deriving (Show, Read, Eq, Typeable)

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

instance ToJSON GameInfo
instance FromJSON GameInfo

instance ToJSON Id where
    toJSON (Id id) = A.String $ T.pack id

instance ToJSON Ok where
    toJSON _ = object ["ok" .= True]

instance FromJSON BotCommand

-- Actions --
instance ToJSON BotAction where
    toJSON = typeToJSON show

instance FromJSON BotAction where
    parseJSON = typeParseJSON readMay


-- Direction
removeFirstLetter = tail
addD cs = 'D':cs

instance ToJSON Direction where
    toJSON = typeToJSON (removeFirstLetter.show)

instance FromJSON Direction where
    parseJSON = typeParseJSON (readMay.addD)

-- State
instance ToJSON BotState where
    toJSON = typeToJSON show

instance FromJSON BotState where
    parseJSON = typeParseJSON readMay

-- Bot
-- sometimes kills exists, sometimes it doesn't
-- 0 means it doesn't matter
instance ToJSON Bot where
  toJSON b = object fs
    where p = point b
          id = botId b
          fs = [ "id" .= id 
               , "x" .= x p
               , "y" .= y p
               , "name" .= name b
               , "player" .= player b
               , "sprite" .= sprite b 
               , "state" .= botState b
               , "kills" .= kills b
               , "created" .= created b
               ]

-- you don't need the other fields from the client. So just make them up with defaults
instance FromJSON Bot where 
    parseJSON (Object v) = do
        x <- v .: "x"
        y <- v .: "y" 
        name <- v .: "name"
        sprite <- v .: "sprite"
        return $ Bot (Point x y) name "" sprite "" "" 0 (fromSeconds 0) Active Nothing

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
    toDoc b = 
        let p = point b in

        [ "x" := val (x p)
        , "y" := val (y p)
        , "name" := val (name b)
        , "player" := val (player b)
        , "sprite" := val (sprite b)
        , "_id" := val (botId b)
        , "playerId" := val (botPlayerId b)
        , "created" := val (created b)
        , "state" := val (botState b)
        , "command" := val (command b)
        ]

instance FromDoc Bot where
    fromDoc d = Bot (Point (at "x" d) (at "y" d))
                    (at "name" d) 
                    (at "player" d) 
                    (at "sprite" d)
                    (fromMaybe "" (lookup "_id" d))
                    (fromMaybe "" (lookup "playerId" d))
                    (fromMaybe 0 (lookup "kills" d))
                    (fromMaybe (fromSeconds 0) (lookup "created" d))
                    (at "state" d)
                    (lookup "command" d)

instance FromDoc Point where
    fromDoc p = Point (at "x" p) (at "y" p)

instance FromDoc Player where
    fromDoc p = Player (fromMaybe "" (lookup "_id" p)) (at "name" p) (at "source" p)

instance ToDoc Player where
    toDoc p = [ "_id" := val (playerId p)
              , "name" := val (playerName p)
              , "source" := val (source p)
              ]

instance Val BotCommand where
    val (BotCommand a d) = val ["action" := val a, "direction" := val d]
    cast' (Doc d) = Just $ BotCommand (at "action" d) (at "direction" d)
    cast' _ = Nothing

instance Val BotAction where
    val = typeToBSON show
    cast' = typeFromBSON readMay

instance Val Direction where
    val = typeToBSON (removeFirstLetter.show)
    cast' = typeFromBSON (readMay.addD)

instance Val BotState where
    val = typeToBSON show
    cast' = typeFromBSON readMay



-- BSON HELPERS ---------------------------------------------------------------

typeToBSON :: (a -> String) -> a -> B.Value
typeToBSON show = val . show

typeFromBSON :: (String -> Maybe a) -> B.Value -> Maybe a
typeFromBSON read (B.String bs) = do
    m <- read $ T.unpack bs
    return m


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

