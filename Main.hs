{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module Main where

import Botland.Types (Unit(..), Actor(..), Point(..))
import Botland.Helpers (decodeBody, body, queryRedis, uuid, l2b, b2l, Fault(..), send)
import Web.Scotty (get, post, json, param, header, scotty)



import Database.Redis (runRedis, connect, defaultConnectInfo, ping, set, keys, Redis, Connection, incr, hset, Reply(..))

import qualified Database.Redis as R 
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.ByteString.Char8 (pack, append, concat)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L


-- delete me
import Data.Aeson (decode, encode)
import Control.Monad (when, guard)
import Network.HTTP.Types (statusBadRequest)

main :: IO ()
main = do
    db <- connect defaultConnectInfo
    let redis = queryRedis db

    scotty 3000 $ do

        -- returns the world location map
        -- get "/world" $ do
        --     w <- world
        --     json w

        get "/actor/:unitId" $ do
            uid <- param "unitId"  
            a <- redis $ actorFetch uid
            send a

        post "/actor/new" $ decodeBody $ \a -> do
            au <- redis $ actorCreate a
            header "X-Unit-Token" $ T.pack (token au)
            json au

        -- mount $ serveDirectory DisableBrowsing ["index.html"] "./public" 



-- changeme: instead I should return error messages on a server error
actorFetch :: B.ByteString -> Redis (Either Fault Actor)
actorFetch uid = do
    reply <- R.get ("units:" `append` uid) 
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
    hset "world" (pack $ show p) (pack id)

    -- so many characters are LAME, pack, show, encode, l2b, append
    -- so, i could pick an internal string representation that matched ONE of them. 
    -- Text, ByteString, or L.ByteString

    -- return all the information
    return $ Token id token a

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


