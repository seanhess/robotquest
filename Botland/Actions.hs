{-# LANGUAGE OverloadedStrings #-}

module Botland.Actions where

import Prelude hiding ((++))

import Botland.Types
import Botland.Helpers (uuid, l2b, b2l)

import Database.Redis (runRedis, connect, defaultConnectInfo, ping, get, set, keys, Redis, Connection, incr, hset, Reply(..), hgetall, hdel, hsetnx)

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
        Right m -> return $ Right $ Field (map toLocation m)

actorFetch :: B.ByteString -> Redis (Either Fault Actor)
actorFetch uid = do
    reply <- get ("units:" ++ uid) 
    case reply of
        Left _ -> return $ Left NotFound
        Right (Just bs) -> do
            let ma = decode $ b2l bs :: Maybe Actor
            case ma of
                Nothing -> return $ Left $ Fault "Could not parse stored actor"
                Just a -> return $ Right a

actorCreate :: Actor -> Redis (Unit Actor) 
actorCreate a = do
    id <- uuid
    token <- uuid

    let p = Point 0 0 -- CHANGEME
    
    -- save the actor information, its token, and its position
    let key = ("units:" ++ (pack id))
    set key (l2b $ encode a)
    set (key ++ ":token") (pack token)
    set (key ++ ":location") (showPoint p)
    hset "world" (showPoint p) (pack id)

    return $ Token id token a


actorMove :: B.ByteString -> Point -> Redis (Either Fault String)
actorMove uid p = do
    let k = ("units:" ++ uid)
        lk = (k ++ ":location")
    ep <- get lk
    case ep of
        Left r -> return $ Left $ Fault (show r)
        Right Nothing -> return $ Left $ Fault "Could not find location"
        Right (Just bs) -> do
            let po = readPoint bs

            if (not $ neighboring p po) then 
                return $ Left $ Fault "Invalid move"
            else do

            res <- hsetnx "world" (showPoint p) uid
            case res of 
                Left _ -> return $ Left $ Fault "Space Occupied"
                Right True -> do
                    set lk (showPoint p)
                    hdel "world" [(showPoint po)] 
                    return $ Right "OK"

neighboring :: Point -> Point -> Bool
neighboring p1 p2 = withinOne (x p1) (x p2) && withinOne (y p1) (y p2)
    where withinOne a b = (a == b || a == (b-1) || a == (b+1))



authorized :: ByteString -> ByteString -> Redis Bool
authorized uid token = do
    r <- get ("units:" ++ uid ++ ":token")
    case r of
        Left r -> return False
        Right (Just bs) -> return (token == bs)


(++) :: ByteString -> ByteString -> ByteString
(++) = append

