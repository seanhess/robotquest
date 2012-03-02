{-# LANGUAGE OverloadedStrings #-}

module Main where

import Botland.Actions (unitSpawn, unitGetDescription, unitMove, authorized, resetWorld, worldLocations, worldInfo) 
import Botland.Types.Unit (unitToken)
import Botland.Types.Message (Fault(..))
--import Botland.Types.Location (Point(..))
import Botland.Helpers (decodeBody, body, queryRedis, uuid, l2b, b2l, b2t, send)
import Botland.Middleware (ownsUnit)

import Network.Wai.Middleware.Headers (cors)

import Web.Scotty (get, post, param, header, scotty, text, request, middleware, file, json, ActionM(..))


import qualified Database.Redis as R 
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString.Char8 (pack, unpack, append, concat)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L

import Network.Wai.Middleware.Static (staticRoot)

import Data.Maybe (fromMaybe)


main :: IO ()
main = do
    db <- R.connect R.defaultConnectInfo
    let redis = queryRedis db
    let unitAuth = ownsUnit db

    scotty 3000 $ do

        middleware $ staticRoot "public"
        middleware cors

        get "/" $ do
            header "Content-Type" "text/html"
            file "public/index.html"

        get "/world" $ do
            send $ Right worldInfo

        get "/world/locations" $ do
            ls <- redis $ worldLocations
            send ls

        get "/unit/:unitId/description" $ do
            uid <- param "unitId"  
            a <- redis $ unitGetDescription uid
            send a

        post "/unit/spawn" $ decodeBody $ \d -> do
            s <- redis $ unitSpawn d 
            header "X-Auth-Token" $ b2t (unitToken s)
            json s

        post "/unit/:unitId/move" $ unitAuth $ decodeBody $ \p -> do
            uid <- param "unitId"
            res <- redis $ unitMove uid p
            send res

        -- temporary, for admin testing
        post "/admin/clear" $ do
            redis $ resetWorld
            text "OK"




