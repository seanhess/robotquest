{-# LANGUAGE OverloadedStrings #-}

module Main where

import Botland.Actions (world, actorFetch, actorCreate)
import Botland.Types (Unit(..), Actor(..), Point(..))
import Botland.Helpers (decodeBody, body, queryRedis, uuid, l2b, b2l, Fault(..), send)
import Web.Scotty (get, post, json, param, header, scotty)

import qualified Database.Redis as R 
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
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

        post "/actor/new" $ decodeBody $ \a -> do
            au <- redis $ actorCreate a
            header "X-Unit-Token" $ T.pack (token au)
            json au

        -- mount $ serveDirectory DisableBrowsing ["index.html"] "./public" 


{-

BOT FIELDS
[ ] home page / repository
[ ] appearance? name? type? (yes, they need a unique typename)

NEXT STEPS
[ ] GET unit information
[ ] GET world with locations
[ ] move unit

REMEMBER
[ ] Unit cleanup (no heartbeat?, etc?)

-}


