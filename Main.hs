{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module Main where

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)

import Happstack.Ella 
import Happstack.Lite

import Happstack.Server (takeRequestBody, askRq)
import Happstack.Server.Types (RqBody(..))

import Data.Maybe (fromJust)

import Data.Data (Data)

import Botland.Types

import qualified Data.Aeson.Generic as AG
import qualified Data.Aeson as A
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as L

main :: IO ()
main = serve Nothing app

app :: ServerPart Response
app = route $ do
    post "/unit/create" createUnit
    mount $ serveDirectory DisableBrowsing ["index.html"] "./public" 

createUnit :: Req -> ServerPart Response
createUnit r = do
    -- easy parsing! Wahoo!
    -- let message = fromJust $ A.decode (body r) :: Message Unit
        -- unit = obj message
    let unit = fromJust $ AG.decode (body r) :: Unit
        -- unit = obj msg 
    ok $ toResponse $ encode (Message unit)
    ok $ toResponse $ encode (Error "This is an error")
    ok $ toResponse $ encode (Token unit "a00s90sdksdisd0sd0sd90sd09")
    -- send unit

-- send :: (Data a) => a -> ServerPart Response
-- send = ok . toResponse . A.encode . Message

-- {error: "error message"}
-- {}


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


THINGS TO EXPERIMENT WITH
[ ] Figure out redis
[ ] Proper way to send down that token along with the message

-}


