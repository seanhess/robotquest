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
game = Game 10 10 1000 

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
        get "/game/locations" $ do
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
        put "/mcps/:mcpId/bots/:botId/action" $ auth $ decodeBody $ \c -> do
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

        -- TODO: cleanup
        -- TODO: pubnub (just POST to pubnub every time something happens. It's easy)
        -- TODO: documentation
        -- TODO: better graphics

        -- TEST
        -- if I do something long-running, it will still respond, correct?
        -- if I post to pubnub, it will delay responses, I think. (but that's ok)

        -- OPTIMIZE
        -- keepalive
        -- varnish

        -- LAUNCH
        -- better home page / logo?
        -- make it bigger
        -- write a couple bots that stay in there, doing something interesting

        -- AFTER LAUNCH
        -- TODO: Add bulk requests? (launch first)
        -- TODO: Encforce movement limit (launch first) 

connectMongo :: IO (Pipe) 
connectMongo = runIOE $ connect (host "127.0.0.1")








