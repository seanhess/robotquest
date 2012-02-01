{-# LANGUAGE OverloadedStrings #-}

module Main where

import Botland.Actions (world, actorFetch, actorCreate, actorMove)
import Botland.Types (Unit(..), Actor(..), Point(..), readPoint)
import Botland.Helpers (decodeBody, body, queryRedis, uuid, l2b, b2l, send)
import Web.Scotty (get, post, json, param, header, scotty, text)

import qualified Database.Redis as R 
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString.Char8 (pack, unpack, append, concat)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L


main :: IO ()
main = do
    db <- R.connect R.defaultConnectInfo
    let redis = queryRedis db

    scotty 3000 $ do

        get "/world" $ do
            w <- redis $ world
            send w

        get "/actor/:unitId" $ do
            uid <- param "unitId"  
            a <- redis $ actorFetch uid
            send a

        -- TODO: maybe I should just have many of them return whatever, and I can merge the json objects. Then I don't need to have an instance of Unit Actor, or anything
        post "/actor/new" $ decodeBody $ \a -> do
            au <- redis $ actorCreate a
            header "X-Unit-Token" $ T.pack (token au)
            json au

        post "/actor/:unitId/move/:p" $ do
            -- TODO: see if they have the right token
            -- middleware, in other worlds
            -- returns not authorized
            uid <- param "unitId"
            ps <- param "p"
            let p = readPoint ps

            res <- redis $ actorMove uid p
            send res
            
-- what if I had a single error type. I always just returned 200 + an error message, and serialized it. 
-- Fault

        -- mount $ serveDirectory DisableBrowsing ["index.html"] "./public" 


{-

BOT FIELDS
[ ] home page / repository
[ ] appearance? name? type? (yes, they need a unique typename)

NEXT STEPS
[√] GET unit information
[√] GET world with locations
[ ] move unit

REMEMBER
[ ] Unit cleanup (no heartbeat?, etc?)

-}


