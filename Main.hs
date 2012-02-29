{-# LANGUAGE OverloadedStrings #-}

module Main where

--, actorFetch, actorCreate, actorMove, authorized)
import Botland.Actions (world, unitCreate) 
import Botland.Types.Unit (Unit(..))
import Botland.Types.Message (Fault(..))
--import Botland.Types.Location (Point(..))
import Botland.Helpers (decodeBody, body, queryRedis, uuid, l2b, b2l, b2t, send)
import Network.Wai.Middleware.Headers (cors)

import Web.Scotty (get, post, param, header, scotty, text, request, middleware, file, json)
import Network.Wai (requestHeaders)

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


    scotty 3000 $ do

        middleware $ staticRoot "public"
        middleware cors

        get "/" $ do
            header "Content-Type" "text/html"
            file "public/index.html"

        get "/world" $ do
            w <- redis $ world
            send w

        --get "/actor/:unitId" $ do
        --    uid <- param "unitId"  
        --    a <- redis $ actorFetch uid
        --    send a

        post "/unit/new" $ decodeBody $ \a -> do
            u <- redis $ unitCreate a
            header "X-Auth-Token" $ b2t (unitToken u)
            json u

        --post "/actor/:unitId/move" $ decodeBody $ \p -> do
        --    uid <- param "unitId"
        --    r <- request

        --    let headers = requestHeaders r
        --        token = fromMaybe "" $ lookup "X-Auth-Token" headers
            
        --    isAuth <- redis $ authorized uid token
        --    if (not isAuth) then
        --        send $ (Left NotAuthorized :: Either Fault String)
        --    else do
            
        --    res <- redis $ actorMove uid p
        --    send res
            

{-

BOT FIELDS
[ ] home page / repository
[ ] appearance? name? type? (yes, they need a unique typename)

REMEMBER
[ ] Unit cleanup (no heartbeat?, etc?)

[ ] Validate token (middleware?)
[ ] Edges of world
[ ] get rid of all strings (use ByteString)
[ ] Need some functional tests

-}


