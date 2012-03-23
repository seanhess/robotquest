{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Botland.Helpers (decodeBody, body, queryRedis, uuid, l2b, b2l, b2t, send)
import Botland.Types
import Botland.Control

import Database.MongoDB (runIOE, connect, access, master, host)

import Control.Monad.IO.Class (liftIO)

import Data.Text.Lazy (Text, pack)

import Network.Wai.Middleware.Headers (cors)
import Network.Wai.Middleware.Static (staticRoot)
import Network.HTTP.Types (status200)
import Web.Scotty (get, post, delete, param, header, scotty, text, request, middleware, file, json, ActionM(..), status)


game :: Game
game = Game 10 10 1000 

main :: IO ()
main = do

    pipe <- runIOE $ connect (host "127.0.0.1")
    let mongo action = liftIO $ access pipe master "botland" action
    mongo ensureIndexes

    scotty 3000 $ do

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
            res <- mongo $ locations
            send "" res 

        -- really, just gives you a session id, but pretend that it matters :)
        -- works because it's a secret number, never sent to anyone
        post "/mcps" $ do
            id <- createMcp
            json id

        post "/mcps/:mcpId/bots" $ decodeBody $ \b -> do
            mcpId <- param "mcpId"
            id <- mongo $ createBot mcpId b
            send "Invalid Starting Location" id










