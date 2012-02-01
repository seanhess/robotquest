{-# LANGUAGE OverloadedStrings #-}

module Botland.Actions where

import Botland.Types
import Botland.Helpers (uuid, l2b, b2l)

import Database.Redis (runRedis, connect, defaultConnectInfo, ping, get, set, keys, Redis, Connection, incr, hset, Reply(..), hgetall, hdel)

import Data.ByteString.Char8 (pack, unpack, append, concat, ByteString(..))
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text.Lazy as T

import Data.Aeson (encode, decode)

world :: Redis (Either Fault Field)
world = do
    reply <- hgetall "world"  
    case reply of
        Left r -> return $ Left (Fault "Could not get world")
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
                _ -> return $ Left $ Fault "Could not parse stored actor"
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
    set (key `append` ":location") (pack $ showPoint p)
    hset "world" (pack $ showPoint p) (pack id)

    return $ Token id token a


actorMove :: B.ByteString -> Point -> Redis (Either Fault String)
actorMove uid p = do
    let k = ("units:" `append` uid)
        lk = (k `append` ":location")
    ep <- get lk
    case ep of
        Left r -> return $ Left $ Fault (show r)
        Right Nothing -> return $ Left $ Fault "Could not find location"
        Right (Just bs) -> do
            let po = readPoint $ unpack bs

            if (not $ neighboring p po) then 
                return $ Left $ Fault "Invalid move"
            else do

            hset "world" (pack $ showPoint p) uid
            set lk (pack $ showPoint p)
            hdel "world" [(pack $ showPoint po)] 
            
            return $ Right "OK"

                -- try to set. 
            -- unset old location

            -- 2 -- validate new location
                -- valid move? Anyone there?
            -- 3 -- set new location
            -- 4 -- unset old location


neighboring :: Point -> Point -> Bool
neighboring p1 p2 = withinOne (x p1) (x p2) && withinOne (y p1) (y p2)
    where withinOne a b = (a == b || a == (b-1) || a == (b+1))
