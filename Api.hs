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
import Web.Scotty

import System.IO


main :: IO ()
main = do

    -- without this you don't see any log output
    hSetBuffering stdout LineBuffering
    putStrLn "Starting RobotQuest"

    pipe <- connectMongo
    let db action = liftIO $ access pipe master "robotquest" action
    let auth = runAuth pipe "robotquest"

    -- setup 
    db ensureIndexes

    -- recurring tasks
    forkIO $ cleanup db
    forkIO $ runTick gameInfo db

    scotty 3026 $ do

        middleware $ staticRoot "public"
        middleware cors

        get "/" $ do
            redirect "/viewer/"

        get "/viewer" $ do
            cache minute
            header "Content-Type" "text/html"
            file "public/viewer/viewer.html"

        get "/docs" $ do
            cache minute
            header "Content-Type" "text/html"
            file "public/docs/docs.html"

        get "/version" $ text "RobotQuest 0.3.0"

        get "/game/info" $ do
            -- cache minute
            gameInfo <- db $ getGameInfo
            sendAction "" gameInfo

        -- returns all the bots, obstacles and whathaveyounots
        -- everything except playerId

        -- this seems LAME
        -- maybe I could get varnish to delete the cache-control field
        -- cache-control expires-at?

        -- well, but I want an update-oriented thing eventually anyway


        get "/game/objects/:tickCount" $ do
            t <- param "tickCount"

            -- DON'T CACHE (without tick count). the browser won't send the request a second later 
            cache second
            res <- db $ gameAndObjects t
            sendActionFault "" res

        -- really, just gives you a session id, but pretend that it matters :)
        -- works because it's a secret number, never sent to anyone
        post "/players" $ decodeBody $ \p -> do
            id <- db $ createPlayer p
            sendActionFault "" id

        get "/players/:name" $ do
            cache minute
            n <- param "name"
            p <- db $ getPlayerByName n
            sendActionMaybe "Could not find player" p

        -- spawn them immediately, don't wait for the tick
        post "/players/:playerId/minions" $ decodeBody $ \b -> do
            pid <- param "playerId"
            id <- db $ createBot gameInfo pid b
            sendActionFault "Invalid Starting Location" id

        get "/minions/:minionId" $ do
            cache second
            id <- param "minionId"
            bot <- db $ botDetails id
            sendActionFault "" bot

        -- LEADERBOARDS (undocumented, the viewer can just use the normal objects call)
        get "/top/killers" $ do
            cache second
            bots <- db $ topKillers
            sendAction "" bots

        get "/top/survivors" $ do
            cache second
            bots <- db $ topSurvivors
            sendAction "" bots


        -- sets the bot's action
        post "/players/:playerId/minions/:minionId/commands" $ auth $ decodeBody $ \c -> do
            mid <- param "minionId"
            pid <- param "playerId"
            res <- db $ setCommand c gameInfo pid mid
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







