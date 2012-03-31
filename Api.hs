{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Botland.Helpers
import Botland.Types
import Botland.Control
import Botland.Middleware

import Database.MongoDB (runIOE, connect, access, master, host, Pipe, Action)

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad.IO.Class (liftIO)

import Data.Text.Lazy (Text, pack)

import Network.Wai.Middleware.Headers (cors)
import Network.Wai.Middleware.Static (staticRoot)
import Network.HTTP.Types (status200)
import Web.Scotty

game :: Game
game = Game 30 30 1000 

cleanupDelay :: Integer
cleanupDelay = 2 

main :: IO ()
main = do

    pipe <- connectMongo
    let db action = liftIO $ access pipe master "botland" action
    let auth = runAuth pipe "botland"
    db ensureIndexes

    let cleanup = do 
        db $ cleanupInactives cleanupDelay
        threadDelay ((fromIntegral cleanupDelay)*1000000)
        cleanup

    -- run cleanup every so often
    forkIO $ cleanup

    scotty 3026 $ do

        middleware $ staticRoot "public"
        middleware cors

        get "/" $ do
            header "Content-Type" "text/html"
            file "public/index.html"

        get "/version" $ text "Botland 0.2.0"

        get "/game" $ json game

        -- returns all the bots, obstacles and whathaveyounots
        -- everything except MCPId
        get "/game/bots" $ do
            res <- db $ locations
            sendAction "" res

        -- really, just gives you a session id, but pretend that it matters :)
        -- works because it's a secret number, never sent to anyone
        post "/mcps" $ do
            id <- db $ createMcp
            sendAction "" id

        -- spawn them immediately, don't wait for the tick
        post "/mcps/:mcpId/bots" $ decodeBody $ \b -> do
            mcpId <- param "mcpId"
            id <- db $ createBot game mcpId b
            --updateHeartbeat mcpId
            sendActionFault "Invalid Starting Location" id

        -- sets the bot's action
        post "/mcps/:mcpId/bots/:botId/command" $ auth $ decodeBody $ \c -> do
            botId <- param "botId"
            mcpId <- param "mcpId"
            res <- db $ performCommand c game mcpId botId
            sendActionFault "Invalid Space: Occupied?" res

        -- delete all bots associated with the mcp
        delete "/mcps/:mcpId" $ do
            mcpId <- param "mcpId"
            ok <- db $ cleanupMcp mcpId
            sendAction "Could not delete mcp" ok

        delete "/mcps/:mcpId/bots/:botId" $ auth $ do
            botId <- param "botId"
            mcpId <- param "mcpId"
            ok <- db $ cleanupBot mcpId botId 
            sendAction "Could not delete bot" ok

        -- TODO: implement planet cute graphics
            -- then start making up mini games
            -- random terrain generation (grow)
            -- spawn some stars. keep track of how many you've collected

        -- TODO: stats! to give it a point. Just list everything
            -- mcps connected the longest
            -- currently connected mcps
            -- bots that survived the longest
            -- resources collected

        -- TODO: resources / blocks (little gold coins?)

        -- TODO: documentation
        -- TODO: better graphics
        -- TODO: figure out what the launch site will be like 

        -- TODO: write some more interesting bots 

        -- TODO: varnish, caching. 

        -- AFTER LAUNCH
        -- TODO: pubnub XXX (requires clients to know all the logic (delete, etc))
        -- TODO: Add bulk requests? (launch first) (need to figure out new locking mechanism)
        -- TODO: Encforce movement limit (launch first) 

        -- XXXX: pipelining won't work. The clients don't really do it.
        -- send an array of requests, and get an array of responses
        -- request: METHOD URL BODY
        -- returns: STATUS BODY
        -- or: create mcpId body
        --     command mcpId botId body
        --     delete mcpId botId body
        -- naw, they already know how to do the routes, just use that. You have to write a parser anyway, and you can copy scotty's
        -- post "/pipeline" $ decodeBody $ \cs -> do 

connectMongo :: IO (Pipe) 
connectMongo = runIOE $ connect (host "127.0.0.1")








