{-# LANGUAGE OverloadedStrings #-}

module Botland.Middleware where

import Botland.Types.Message (Fault(..))

import Botland.Helpers (queryRedis, send)
import Botland.Actions (authorized)

import Data.Maybe (fromMaybe)
import Database.Redis (Connection)

import Network.Wai (requestHeaders)
import Web.Scotty (ActionM, request, raise, status, text, redirect, rescue, header, json, param)


ownsUnit :: Connection -> ActionM () -> ActionM ()
ownsUnit db k = do
    uid <- param "unitId"
    r <- request

    let headers = requestHeaders r
        token = fromMaybe "" $ lookup "X-Auth-Token" headers

    isAuth <- queryRedis db $ authorized uid token
    if (not isAuth) then
        send $ (Left NotAuthorized :: Either Fault String)
        else k 