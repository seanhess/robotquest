{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Botland.Actions (unitSpawn, unitGetDescription, unitMove, authorized, resetWorld, worldLocations, heartbeat, removeUnit)
import Botland.Types.Unit (unitToken)
import Botland.Types.Message (Fault(..), Test(..))
import Botland.Types.Location (Point(..), Size(..), FieldInfo(..), GameInfo(..))
import Botland.Helpers (decodeBody, body, queryRedis, uuid, l2b, b2l, b2t, send)
import Botland.Middleware (ownsUnit)

import Network.Wai.Middleware.Headers (cors)

import Web.Scotty (get, post, delete, param, header, scotty, text, request, middleware, file, json, ActionM(..), status)


import qualified Database.Redis as R 
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString.Char8 (pack, unpack, append, concat)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L

import Network.Wai.Middleware.Static (staticRoot)
import Network.HTTP.Types (status200)

import Data.Maybe (fromMaybe)


import Control.Monad.IO.Class (liftIO)

import Botland.Types.Unit (SpawnRequest)

worldInfo :: FieldInfo
worldInfo = FieldInfo (Point 0 0) (Size 10 10)

gameInfo :: GameInfo
gameInfo = GameInfo worldInfo 500 

main :: IO ()
main = do
    db <- R.connect R.defaultConnectInfo
    let redis = queryRedis db
    let unitAuth = ownsUnit db

    scotty 3000 $ do

        middleware $ staticRoot "public"
        middleware cors

        get "/" $ do
            header "Content-Type" "text/html"
            file "public/index.html"

        get "/version" $ do
            liftIO $ putStrLn "0.1.3"
            text "0.1.3"

        post "/test" $ decodeBody $ \(test :: Test) -> do 
            liftIO $ print test
            text "OK"

        get "/game" $ do
            send $ Right gameInfo

        get "/world" $ do
            send $ Right worldInfo

        get "/world/locations" $ do
            ls <- redis $ worldLocations
            send ls

        get "/units/:unitId/description" $ do
            uid <- param "unitId"  
            a <- redis $ unitGetDescription uid
            send a

        -- put "/units/:unitId/description"
        -- delete "/units/:unitId"

        post "/units" $ decodeBody $ \(sr :: SpawnRequest) -> do
            res <- redis $ unitSpawn worldInfo sr 
            case res of
                Right s -> do
                    header "X-Auth-Token" $ b2t (unitToken s)
                    send res
                _ -> send res

        delete "/units/:unitId" $ unitAuth $ do
            uid <- param "unitId"
            redis $ removeUnit uid
            status status200

        post "/units/:unitId/heartbeat" $ unitAuth $ do
            uid <- param "unitId"
            redis $ heartbeat uid
            status status200
            
        post "/units/:unitId/move" $ unitAuth $ decodeBody $ \p -> do
            uid <- param "unitId"
            res <- redis $ unitMove worldInfo uid p
            send res






        -- temporary, for admin testing. 
        post "/admin/clear" $ do
            redis $ resetWorld
            status status200



