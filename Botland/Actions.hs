{-# LANGUAGE OverloadedStrings #-}

module Botland.Actions where

import Botland.Types
import Botland.Helpers (uuid, l2b, b2l, Fault(..))

import Database.Redis (runRedis, connect, defaultConnectInfo, ping, get, set, keys, Redis, Connection, incr, hset, Reply(..), hgetall)

import Data.ByteString.Char8 (pack, unpack, append, concat, ByteString(..))
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L

import Data.Aeson (encode, decode)

world :: Redis (Either Fault Field)
world = do
    reply <- hgetall "world"  
    case reply of
        Left r -> return $ Left (ServerError "Could not get world")
        Right m -> return $ Right $ Field (map toField m)
    where toField (l, v) = ((readPoint $ unpack l), (unpack v))
    

actorFetch :: B.ByteString -> Redis (Either Fault Actor)
actorFetch uid = do
    reply <- get ("units:" `append` uid) 
    case reply of
        Right (Just bs) -> do
            let ma = decode $ b2l bs :: Maybe Actor
            case ma of
                Just a -> return $ Right a
                _ -> return $ Left $ ServerError "Could not parse stored actor"
        _ -> return $ Left NotFound

actorCreate :: Actor -> Redis (Unit Actor) 
actorCreate a = do
    id <- uuid
    token <- uuid

    let p = Point 0 0 -- CHANGEME
    
    -- save the actor information, its token, and its position
    let key = ("units:" `append` (pack id))
    set key (l2b $ encode a)
    set (key `append` ":token") (pack token)
    hset "world" (pack $ showPoint p) (pack id)

    return $ Token id token a
