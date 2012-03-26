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

main :: IO ()
main = do

    pipe <- connectMongo
    let db action = liftIO $ access pipe master "botland" action
    let auth = runAuth pipe "botland"
    db ensureIndexes

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
            send "" res 

        -- really, just gives you a session id, but pretend that it matters :)
        -- works because it's a secret number, never sent to anyone
        post "/mcps" $ do
            id <- createMcp
            json id

        -- spawn them immediately, don't wait for the tick
        post "/mcps/:mcpId/bots" $ decodeBody $ \b -> do
            mcpId <- param "mcpId"
            id <- db $ createBot mcpId b
            send "Invalid Starting Location" id

        -- sets the bot's action
        put "/mcps/:mcpId/bots/:botId/action" $ auth $ decodeBody $ \c -> do
            botId <- param "botId"
            res <- db $ setAction botId (action c)
            send "Could not set action" res

        -- TODO: real tests - api + functional
        -- TODO: attack
        -- TODO: better, less obtrusive bot harness (not the same as the viewer?)

        -- AFTER LAUNCH
        -- TODO: Add bulk requests? (launch first)
        -- TODO: Encforce movement limit (launch first) 

connectMongo :: IO (Pipe) 
connectMongo = runIOE $ connect (host "127.0.0.1")







