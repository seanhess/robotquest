{-# LANGUAGE OverloadedStrings #-}

module Main where

import Botland.Actions (world, actorFetch, actorCreate, actorMove, authorized)
import Botland.Types (Unit(..), Actor(..), Point(..), readPoint, Fault(..))
import Botland.Helpers (decodeBody, body, queryRedis, uuid, l2b, b2l, send)
import Web.Scotty (get, post, json, param, header, scotty, text, request)
import Network.Wai (requestHeaders)

import qualified Database.Redis as R 
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString.Char8 (pack, unpack, append, concat)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L


import Data.Maybe (fromMaybe)


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

            uid <- param "unitId"
            ps <- param "p"
            r <- request
            let headers = requestHeaders r
                token = fromMaybe "" $ lookup "X-Unit-Token" headers
            
            isAuth <- redis $ authorized uid token
            if (not isAuth) then
                send $ (Left NotAuthorized :: Either Fault String)
            else do
            
            let p = readPoint ps
            
            res <- redis $ actorMove uid p
            send res
            
        -- mount $ serveDirectory DisableBrowsing ["index.html"] "./public" 



{-

BOT FIELDS
[ ] home page / repository
[ ] appearance? name? type? (yes, they need a unique typename)

REMEMBER
[ ] Unit cleanup (no heartbeat?, etc?)

[ ] Validate token (middleware?)
[ ] Edges of world
[ ] get rid of all strings (use ByteString)

-}


