{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Botland.Helpers (decodeBody, body, queryRedis, uuid, l2b, b2l, b2t, send)
import Botland.Types (Bot(..), Game(..))

import Control.Monad.IO.Class (liftIO)

import Network.Wai.Middleware.Headers (cors)
import Network.Wai.Middleware.Static (staticRoot)
import Network.HTTP.Types (status200)
import Web.Scotty (get, post, delete, param, header, scotty, text, request, middleware, file, json, ActionM(..), status)


game :: Game
game = Game 10 10 1000 

main :: IO ()
main = do
    scotty 3000 $ do

        middleware $ staticRoot "public"
        middleware cors

        get "/" $ do
            header "Content-Type" "text/html"
            file "public/index.html"

        get "/version" $ text "Botland 0.2.0"

        get "/game" $ json game

        --get "/world" $ do
        --    send $ Right worldInfo

        --get "/world/locations" $ do
        --ls <- redis $ worldLocations
        --json ls
        --send ls




