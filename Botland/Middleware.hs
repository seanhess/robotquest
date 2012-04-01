{-# LANGUAGE OverloadedStrings #-}

module Botland.Middleware where

import Botland.Types hiding (Direction(..))
import Botland.Control
import Botland.Helpers

import Control.Monad.IO.Class (liftIO)

import Data.Aeson (FromJSON, decode)
import Data.ByteString.Lazy.Char8 (ByteString, unpack)

import Database.MongoDB (Action, Pipe, access, master, Database)

import Network.HTTP.Types (status400)

import Web.Scotty

-- lets you make sure they have control over a bot before letting them play with it
runAuth :: Pipe -> Database -> ActionM () -> ActionM ()
runAuth pipe d k = do
    let mongo action = liftIO $ access pipe master d action 

    mid <- param "minionId"
    pid <- param "playerId"
    ok <- mongo $ botOwner pid mid
    case ok of
        Right True -> k
        _ -> fault NotAuthorized


-- parse the body as something, and call "k" with the result
decodeBody :: (FromJSON a) => (a -> ActionM ()) -> ActionM ()
decodeBody k = do
    b <- body
    let mo = decode b
    case mo of
        Just o -> k o  
        Nothing -> do
            status status400
            json $ Fault ("Invalid Body JSON: " ++ (unpack b))
