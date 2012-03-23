{-# LANGUAGE OverloadedStrings #-}

module Botland.Helpers where

import Prelude hiding ((++))

import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Error (throwError)
import qualified Control.Monad.State as MS 

import Database.MongoDB (Failure(..))

import Botland.Types.Message (Fault(..))

import Data.Aeson (decode, ToJSON, FromJSON, encode)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString as B
import Data.ByteString.Char8 (ByteString, pack, unpack, append)
import Data.Conduit.Lazy (lazyConsume)
import qualified Data.Text.Lazy as T

import Network.HTTP.Types (statusBadRequest, status404, status500, status400, status401)
import Network.Wai (requestBody)

import Web.Scotty (ActionM, request, raise, status, text, redirect, rescue, header, json, param)
import qualified Web.Scotty

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
    b <- body
    let mo = decode b
    case mo of
        Just o -> k o  
        Nothing -> do
            --liftIO $ print b
            status statusBadRequest
            json $ Fault "Could not parse body"

queryRedis :: Connection -> Redis a -> ActionM a
queryRedis db r = liftIO $ runRedis db r

uuid :: Redis ByteString
uuid = do
    u <- liftIO $ U.uuid
    return $ pack $ show u


-- AUTHENTICATION


-- converts a lazy bytestring to redis bytestring
l2b :: L.ByteString -> B.ByteString
l2b = B.concat . L.toChunks

b2l :: B.ByteString -> L.ByteString
b2l l = L.fromChunks [l]

b2t :: B.ByteString -> T.Text
b2t = T.pack . unpack 

-- handles the fault checking, sending proper stuff
send :: (ToJSON a) => ByteString -> Either Failure a -> ActionM ()
send m ea = do
    case ea of
        Left (DocNotFound _) -> 
            status status404
        Left (QueryFailure _ _) -> do
            status status400
            json $ Fault m
        Left (WriteFailure _ _) -> do
            status status400
            json $ Fault m
        Left f -> do
            status status500
            json $ Fault "Database Error"
        Right a -> json a 

(++) :: ByteString -> ByteString -> ByteString
(++) = append


