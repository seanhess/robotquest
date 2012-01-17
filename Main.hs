{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module Main where

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)

import Happstack.Ella as E 
import Happstack.Lite

import Happstack.Server (takeRequestBody, askRq)
import Happstack.Server.Types (RqBody(..))

import Data.Maybe (fromJust)

import Data.Data (Data)

import Botland.Types

import Data.Aeson.Generic (encode, decode)
import qualified Data.ByteString.Lazy.Char8 as L

main :: IO ()
main = serve Nothing app

app :: ServerPart Response
app = route $ do
    post "/unit/create" createUnit
    mount $ serveDirectory DisableBrowsing ["index.html"] "./public" 

-- screw it, just use headers for the token
createUnit :: E.Req -> ServerPart Response
createUnit r = do
    let unit = fromJust $ decode (body r) :: Unit
    ok $ toResponse $ encode (Error "This is an error")


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


