{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module Main where

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)

import Happstack.Ella (Req, body, cap, route, get, post, mount)
import Happstack.Server (ok, toResponse, ServerPart, Response, serveDirectory, Browsing(..), internalServerError, setHeaderM)
import Happstack.Server.Monads (ServerPartT)

import Happstack.Lite (serve)

import Happstack.Server (takeRequestBody, askRq)
import Happstack.Server.Types (RqBody(..))

import Data.Data (Data)

import Botland.Types

import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy.Char8 as L

import Database.Redis (runRedis, connect, defaultConnectInfo, ping, set, keys, Redis, Connection)
import qualified Database.Redis as R 

import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans.Class (lift)

-- This is the right format. No a included, because it gets man later
type MyMonad = ReaderT L.ByteString (ServerPartT IO) 

-- file: ch18/UglyStack.hs
runMyMonad :: MyMonad a -> L.ByteString -> ServerPart a
runMyMonad k s = runReaderT k s

-- Ok, there is no RedisT, but there is a ServerPartT
-- ServerPartT ReaderT MyConfig (Redis IO)

main :: IO ()
main = do
    db <- connect defaultConnectInfo
    runRedis db doSomeStuff  -- i can do stuff, but I want an AWESOME monad ...
    -- let wt = "woot" :: L.ByteString
    -- let router = simple :: ServerPart Response
    -- serve Nothing simple
    -- serve Nothing $ runReaderT simple wt
    serve Nothing $ runMyMonad simple "WHATUP!"


doSomeStuff :: Redis ()
doSomeStuff = do
    result <- ping
    set "mykey" "myvalue"
    value <- R.get "mykey"
    k <- keys "*"
    liftIO $ print k
    -- case result of 
    --     Left reply -> liftIO $ print reply
    --     Right status -> liftIO $ print status 
    return ()

simple :: MyMonad Response
simple = do
    foo <- ask
    ok $ toResponse foo
    -- let hi = "HELLO" :: L.ByteString
    -- ok $ toResponse "HELLO"
    -- value <- ask -- gets the value out, no?  -- not an instance of MonadReader
    -- return $ ok $ toResponse ask
    -- ok $ toResponse "HELLO"
    -- return toResponse "HELLO"

app :: ServerPart Response
app = route $ do
    get "/unit/:unitId" unitDetails
    post "/creature/create" createCreature
    mount $ serveDirectory DisableBrowsing ["index.html"] "./public" 

unitDetails :: Req -> ServerPart Response
unitDetails r = do
    let unitId = cap r
    ok $ toResponse $ encode (Error ("not implemented, but you gave us the id: " ++ unitId))

createCreature :: Req -> ServerPart Response
createCreature r = do
    let creature = decode (body r) :: Maybe Creature
    case creature of 
        Nothing -> internalServerError $ toResponse $ encode $ Error "Could not parse your creature"
        Just c -> do
            -- put him in the data store
            let unitId = "id"
                unitToken = "abcdefg"
            setHeaderM "X-Unit-Token" unitToken
            ok $ toResponse $ encode $ CreatureUnit unitId c

{-

CREATE A UNIT
[√] POST /unit/create
[ ] Creates it in the data store
[ ] Figure out data storage (redis, mongo?)
[ ] Create a unit id
[ ] Create a unit control token
[√] Send down the unit (body)
[ ] Send down control token (header? extra body field?)

[ ] Protocol: {unitToken: "", unit: {}}, rather than headers, because headers are hard for javascript. 
[ ] {error: "", unit{}}, etc, rather than simply body messages. It's just easier to deal with.

[ ] Bots can have a home page or source link

THINGS TO EXPERIMENT WITH
[ ] Figure out redis
[ ] Proper way to send down that token along with the message

-}


