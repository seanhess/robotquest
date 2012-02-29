{-# LANGUAGE OverloadedStrings #-}

module Botland.Helpers where

import Botland.Types

import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Error (throwError)

import Data.Aeson (FromJSON, encode, decode, ToJSON)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString as B
import Data.Conduit.Lazy (lazyConsume)
import qualified Data.Text.Lazy as T

import Network.HTTP.Types (statusBadRequest, status404, status500, status400, status401)
import Network.Wai (requestBody)

import Web.Scotty (ActionM, request, raise, status, json, text, redirect, rescue)

import Database.Redis (runRedis, connect, defaultConnectInfo, ping, set, keys, Redis, Connection, incr)

import qualified System.UUID.V4 as U

-- i need to turn this into something json compatible
body :: ActionM (L.ByteString)
body = do
    r <- request
    bss <- liftIO . runResourceT . lazyConsume . requestBody $ r
    return $ L.fromChunks bss

-- this style of function isn't really going to work well. 
-- because I can't leap out and do something fancy (unfortunately)
decodeBody :: (FromJSON a) => (a -> ActionM ()) -> ActionM ()
decodeBody k = do
    -- return ()
    -- return $ Creature Player
    b <- body
    let mo = decode b
    case mo of
        Just o -> k o  
        Nothing -> do
            status statusBadRequest
            text "Could not parse body"

queryRedis :: Connection -> Redis a -> ActionM a
queryRedis db r = liftIO $ runRedis db r

uuid :: Redis String
uuid = do
    u <- liftIO $ U.uuid
    return $ show u


-- converts a lazy bytestring to redis bytestring
l2b :: L.ByteString -> B.ByteString
l2b = B.concat . L.toChunks

b2l :: B.ByteString -> L.ByteString
b2l l = L.fromChunks [l]

-- handles the fault checking, sending proper stuff
send :: (ToJSON a) => Either Fault a -> ActionM ()
send ea = do
    case ea of
        Left NotFound -> do
            status status404
            json $ Fault "Not Found"
        Left NotAuthorized -> do
            status status401
            json $ Fault "Not Authorized"
        Left f -> do
            status status400 -- always a bad request. Their fault right? :)
            json f
        Right a -> json a 
