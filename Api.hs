{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Botland.Helpers
import Botland.Types
import Botland.Control
import Botland.Middleware
import Botland.Tick

import Database.MongoDB (runIOE, connect, access, master, host, Pipe, Action)

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad.IO.Class (liftIO)

import Data.Text.Lazy (Text, pack)

import Network.Wai.Middleware.Headers (cors)
import Network.Wai.Middleware.Static (staticRoot)
import Network.HTTP.Types (status200)
import Web.Scotty

main :: IO ()
main = do

    pipe <- connectMongo
    let db action = liftIO $ access pipe master "botland" action
    let auth = runAuth pipe "botland"

    -- recurring tasks
    forkIO $ cleanup db
    forkIO $ runTick gameInfo db

    scotty 3026 $ do

        middleware $ staticRoot "public"
        middleware cors

        get "/" $ do
            header "Content-Type" "text/html"
            file "public/index.html"

        get "/viewer" $ do
            header "Content-Type" "text/html"
            file "public/viewer/viewer.html"

        get "/version" $ text "Botland 0.2.0"

        get "/game" $ json gameInfo

        -- returns all the bots, obstacles and whathaveyounots
        -- everything except playerId
        get "/game/minions" $ do
            res <- db $ locations
            sendAction "" res

        -- really, just gives you a session id, but pretend that it matters :)
        -- works because it's a secret number, never sent to anyone
        post "/players" $ decodeBody $ \p -> do
            id <- db $ createPlayer p
            sendAction "" id

        get "/players/:name" $ do
            n <- param "name"
            p <- db $ getPlayerByName n
            sendActionMaybe "Could not find player" p

        -- spawn them immediately, don't wait for the tick
        post "/players/:playerId/minions" $ decodeBody $ \b -> do
            pid <- param "playerId"
            id <- db $ createBot gameInfo pid b
            sendActionFault "Invalid Starting Location" id

        get "/minions/:minionId" $ do
            id <- param "minionId"
            bot <- db $ botDetails id
            sendActionFault "" bot

        -- leaderboards
        get "/top/killers" $ do
            bots <- db $ topKillers
            sendAction "" bots

        get "/top/survivors" $ do
            bots <- db $ topSurvivors
            sendAction "" bots


        -- sets the bot's action
        post "/players/:playerId/minions/:minionId/command" $ auth $ decodeBody $ \c -> do
            mid <- param "minionId"
            pid <- param "playerId"
            res <- db $ performCommand c gameInfo pid mid
            sendAction "" res

        -- delete all bots associated with the player
        delete "/players/:playerId" $ do
            pid <- param "playerId"
            ok <- db $ cleanupPlayer pid
            sendAction "Could not delete player" ok

        delete "/players/:playerId/minions/:minionId" $ auth $ do
            mid <- param "minionId"
            ok <- db $ cleanupBot mid 
            sendAction "Could not delete minion" ok

        -- TODO: implement planet cute graphics
            -- then start making up mini games
            -- random terrain generation (grow)
            -- spawn some stars. keep track of how many you've collected

        -- TODO: stats! to give it a point. Just list everything
            -- player connected the longest
            -- currently connected player
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
        -- or: create player body
        --     command playerId botId body
        --     delete playerId botId body
        -- naw, they already know how to do the routes, just use that. You have to write a parser anyway, and you can copy scotty's
        -- post "/pipeline" $ decodeBody $ \cs -> do 









