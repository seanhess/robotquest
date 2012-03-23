{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Botland.Types where

import qualified Data.Aeson as A
import Data.Aeson (FromJSON(..), ToJSON(..), object, (.=), (.:), Value(..))
import qualified Data.Aeson.Types as AT
import Data.Aeson.Types (Parser)
import Data.Maybe (fromMaybe, isJust, fromJust)
import qualified Data.Text as T

import Database.MongoDB (val, Document, Field(..), at, lookup)

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero, guard)

import GHC.Generics (Generic)

import Prelude hiding (lookup)

import Safe (readMay)


-- DATA TYPES ---------------------------------------------------------------

-- this is roughly what it looks like in the database
--{x, y, _id, color, mcpId, name, source}
data Bot = Bot { x :: Int
               , y :: Int
               , name :: String
               , source :: String
               , color :: String
               , botAction :: BotAction
               , botId :: Maybe String
               , mcpId :: Maybe String
               } deriving (Show)

-- gives the field and interval
data Game = Game { width :: Int
                 , height :: Int
                 , tick :: Int 
                 } deriving (Show, Generic)


-- available actions
data BotAction = Stop | MoveLeft | MoveRight | MoveUp | MoveDown | Invalid deriving (Show)

-- things that can go wrong
data Fault = Fault String
           | NotFound
           | NotAuthorized
           deriving (Generic, Show)

-- when I just want to send back an id
data Id = Id { id :: String } deriving (Show, Generic)

-- sending an action
data Command = Command { action :: BotAction } deriving (Show, Generic)

-- just means the server was successful
data Ok = Ok deriving (Show)






-- JSON SUPPORT --------------------------------------------------------------

instance ToJSON Game
instance FromJSON Game

instance ToJSON Id
instance FromJSON Id

instance ToJSON Command
instance FromJSON Command 

instance ToJSON Ok where
    toJSON _ = object ["ok" .= True]

-- BotAction --
instance ToJSON BotAction where 
    toJSON a = A.String $ T.pack (showAction a)

instance FromJSON BotAction where
    parseJSON (A.String s) = do
        let a = readAction $ T.unpack s
        case a of 
          Invalid -> mzero 
          _ -> return a

showAction :: BotAction -> String
showAction Stop = "stop"
showAction MoveLeft = "left"
showAction MoveRight = "right"
showAction MoveUp = "up"
showAction MoveDown = "down"
showAction _ = error "Tried to show invalid"

readAction :: String -> BotAction
readAction s  
  | s == "stop" = Stop
  | s == "left" = MoveLeft
  | s == "right" = MoveRight
  | s == "up" = MoveUp
  | s == "down" = MoveDown
  | otherwise = Invalid

-- Bot --
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

instance FromJSON Bot where 
    parseJSON (Object v) = do
        x <- v .: "x"
        y <- v .: "y" 
        name <- v .: "name"
        source <- v .: "source"
        color <- v .: "color"
        return $ Bot x y name source color Stop Nothing Nothing
        -- you don't have read the action, mcpId or id from the client, ever.

    parseJSON _ = mzero

-- fault 
instance FromJSON Fault where
    parseJSON (Object v) = Fault <$> v .: "message"

instance ToJSON Fault where
    toJSON f = object ["message" .= message f]

message :: Fault -> String 
message NotFound = "Not Found"
message NotAuthorized = "Not Authorized"
message (Fault m) = m




-- MONGODB -----------------------------------------------------------------
toDoc :: Bot -> Document
toDoc b = [ "x" := val (x b)
          , "y" := val (y b)
          , "name" := val (name b)
          , "source" := val (source b)
          , "color" := val (color b)
          , "_id" := val (fromMaybe "" (botId b))
          , "mcpId" := val (fromMaybe "" (mcpId b))
          , "action" := val (showAction (botAction b))
          ]


-- TODO: actually read action
fromDoc :: Document -> Bot
fromDoc d = Bot (at "x" d) (at "y" d) (at "name" d) (at "source" d) (at "color" d) Stop (lookup "_id" d) (lookup "mcpId" d)






-- SERVER MESSAGES ----------------------------------------------------------








-- JSON HELPERS ---------------------------------------------------------------

typeToJSON :: (Show a) => a -> A.Value
typeToJSON = A.String . T.pack . show

typeParseJSON :: (Read a) => A.Value -> AT.Parser a
typeParseJSON (A.String t) = do
    let mt = readMay $ T.unpack t
    guard (isJust mt)
    return $ fromJust mt
typeParseJSON _ = mzero 
